# Rung 3 — the upstream ask to yoga-postgres: expose referenced columns

**Audience:** Mark Eibes / rowtype-yoga maintainers.
**Status:** design proposal, seeking a decision on approach before a PR.
**Grounded in:** `purescript-yoga-postgres` at merge commit `35e8250`
(the harmonized build notothenia's bridge already consumes). Line numbers
below refer to `src/Yoga/Postgres/Schema.purs` at that commit.

## What notothenia wants to build

Rung 3 of notothenia's extension ladder is **dead-column detection**:
given a schema and the manifest of typed queries an application runs
against it, report the columns *no query ever references*. Those are
candidates for dropping in a migration — and rung 4 already proves that
once they're dropped, any straggler query that did reference them fails
to compile (see `bridge/RUNG4.md`).

Dead-column detection is the dual of rung 4. Rung 4 asks "does this query
still resolve against the new schema?" — which the compiler already
answers. Rung 3 asks "is this column reached by *any* query?" — which
requires the **set of columns a query references**, across all clauses.

yoga is the right layer to answer this, because yoga's type-level parser
already *computes* that set while resolving the query — it just doesn't
keep it.

## The gap: a finished `Q` doesn't retain referenced columns

```purescript
-- Schema.purs:2818
newtype Q :: Row (Row Type) -> Row Type -> Row Type -> Row Type -> Type
newtype Q tables result params stage = Q { sql :: String, values :: Array PG.PGValue }
```

The four type parameters are `tables` (the full schema), `result`,
`params`, `stage`. The raw clause symbols — `select @"name, email"`,
`where_ @"id = $id"` — are reflected into the runtime `sql` string
(`Schema.purs:2882`, `:2936`) and **discarded at the type level**. What
survives is only the *derived* products:

- **`result`** (`ParseSelect`, `:782`) — keyed by *output name*: the
  column name normally, but the **alias** when `... AS x` is used
  (`ParseSelectHandleAS`, `:885`, conses `alias`). So `result` is
  projection reach, but lossy under aliasing — and it says nothing about
  columns referenced only in WHERE / JOIN / ORDER BY / GROUP BY / HAVING.

- **`params`** (`ParseWhere`, `:1487`) — keyed by *param name*: `$id`
  becomes label `id` typed to the compared column's type
  (`FlushWhereWordByHead "$" …`, conses `paramName currentType`,
  `:1584`). The **column's identity is gone** — only its type survives,
  attached to the param. `where_ @"id = $x"` yields `( x :: Int )`; the
  fact that column `id` was referenced is unrecoverable.

So from a finished `Q` you can approximate *projection* reach (via
`result`, modulo aliasing) but you cannot recover the **full set of
referenced columns**. That set is exactly what dead-column detection
needs, and it does not exist in the public type.

## The keystone: every column reference funnels through `ResolveColumn`

The good news — and the reason this is a tractable, *additive* change
rather than a rewrite — is that yoga already routes **every** column
reference, in every clause, through a single class:

```purescript
-- Schema.purs:689
class ResolveColumn :: Symbol -> Row (Row Type) -> Type -> Constraint
class ResolveColumn word tables typ | word tables -> typ
```

- SELECT columns resolve here (`ParseSelectGo` `:803`/`:831`,
  `ParseSelectHandleAS` `:851`).
- WHERE columns resolve here: `FlushWhereWord` excludes keywords
  (`AND`/`OR`/`IS`/`NULL`…) and functions, routes `$params` and literals
  away, and every remaining identifier falls through to
  `ResolveColumn word tables entry` (the `FlushWhereWordA..Z` chain, first
  at `:1639`).
- ORDER BY / GROUP BY / HAVING resolve their columns the same way.

`ResolveColumn` is therefore the one place where "this token is a real
column of the schema" is decided — *after* all of yoga's keyword,
function, parameter, and literal exclusions have already fired. Anything
hung at this point inherits that classification **for free** and stays
correct as the parser grows.

## Proposal

Surface, on a finished `Q`, the set of columns the query references. Two
designs, with a recommendation.

### Option A (recommended) — a `reached` accumulator fed at `ResolveColumn`

Add a fifth parameter to `Q`:

```purescript
newtype Q tables result params stage reached = Q { sql :: String, values :: Array PG.PGValue }
--                                    ^^^^^^^
--   reached :: Row Type — labels are the columns the query references,
--   canonically keyed "<table>.<column>" (the "row-as-set" idiom; the
--   value type is irrelevant, e.g. all Unit)
```

Thread a `reached` row through the parser chains (`ParseSelectGo`,
`ParseWhereGo`/`FlushWhereWord`, the ORDER/GROUP/HAVING walkers),
`Row.Cons`'ing one entry **at each `ResolveColumn` site**. The natural,
join-safe key is the fully-qualified `"<tableName>.<columnName>"`: the
qualified branch (`ResolveColumnBranch True table col …`, `:706`) has
both symbols directly; the unqualified branch (`:713`) discovers the
owning table via `FindUnqualifiedColumn` (`:719`), so the canonical key
is derivable in both cases.

- **Correct by construction.** Reuses yoga's own keyword/function/param/
  literal exclusions — `reached` can never contain a non-column token,
  and it covers *every* clause automatically, including ones added later.
- **Cost.** Changes `Q`'s kind (a breaking change in principle) and
  touches the `Parse*Go` / `FlushWhereWord` chains to thread an extra
  accumulator. Mitigation: put `reached` **last** so the common path —
  code that never writes `Q` explicitly and lets
  `from … # select … # where_ …` infer — is unaffected; only explicit
  `Q` annotations (like notothenia's manifest) gain a parameter.

### Option B (lower-risk increment) — retain clause symbols + a standalone collector

Leave the `Parse*Go` chains untouched. Instead:

1. Have `Q` retain the raw clause strings as phantom `Symbol` parameters
   (pure metadata; existing instances ignore them). Each combinator
   already has its clause symbol in hand at `:2882`/`:2936`/etc. — it
   just appends it to a retained "clauses" symbol.
2. Add a self-contained `ReferencedColumns clauses tables :: Row Type`
   class that re-walks the retained clauses, reusing the existing
   `SkipSpaces` / `ExtractWord` / `SplitOnDot` / `ResolveColumn`
   primitives to emit the `reached` row.

- **Lower blast radius** on the delicate mutually-recursive parser
  instances — they're reused read-only.
- **Cost.** Still changes `Q`'s kind, and the collector must re-derive
  "is this token a column?" — risking divergence from the real parser on
  aggregates, `AS`, functions, and qualified refs. (Option A can't
  diverge because it *is* the real parser.)

### Option C (rejected) — consumer restates the clauses

A `ReferencedColumns (select :: Symbol) (where :: Symbol) … tables` that
notothenia feeds the clause literals to directly, without any `Q` change.
Rejected: it duplicates every clause string at the call site, can't
observe what the *actual* `Q` used, and drifts silently. Not worth it.

## Consumer contract notothenia would rely on

A finished `Q … reached` (Option A) or `ReferencedColumns … :: reached`
(Option B) where `reached :: Row Type` has one label per referenced
column, canonically keyed `"<table>.<column>"`. notothenia then computes,
per table:

```
deadColumns(table) = { c ∈ table.columns } ∖ { c | "table.c" ∈ reached over all queries }
```

The schema side notothenia already has (rung-1 codegen emits the
`Table` types from DDL); this ask is purely about the *query* side.

## Edge cases to settle together

These determine the precise semantics of `reached`; flagging rather than
pre-deciding, since they're yours to call:

1. **`SELECT *` / `selectAll`** (`:2865`) reaches every column of the
   table. Should `reached` enumerate them, or carry a wildcard marker
   notothenia expands against `tables`? (Enumerating is simpler for the
   consumer; a marker is cheaper at the type level.)
2. **Aggregate args** — `count(id)` references `id`; the aggregate
   parser (`ResolveAggregateArgCol`, `:1064`) already resolves it, so a
   `ResolveColumn`-sited accumulator (Option A) catches it for free.
3. **`AS` aliases** — `reached` must record the *underlying column*, not
   the alias (unlike `result`, which records the alias). Option A gets
   this right by construction (the accumulator fires at `ResolveColumn`,
   before aliasing); Option B must be careful.
4. **Qualification** — keying everything `"<table>.<column>"` keeps joins
   unambiguous and lets notothenia match by splitting on the dot. Agreed
   as the canonical form?
5. **JOIN `ON` columns** (`innerJoin`, `:3266`) — referenced and should
   count as reached; confirm the join `ON` clause routes through
   `ResolveColumn` like the others.

## Offer

Happy to write the PR for whichever option you prefer — A is the
correct-by-construction one but touches the parser chains; B is the
smaller diff but trades some fidelity. You own the parser and know where
its delicate spots are, so I'd rather get your read on the approach than
guess. Same working arrangement as the dependency-harmonization PR (#1).

Until then, rung 3 stays scoped-but-unbuilt on notothenia's side; rung 4
(which needs none of this) is done and continuously built in `bridge/`.
