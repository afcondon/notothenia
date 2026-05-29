# Notothenia — Synthesis & Lineage

*A note to a future Claude (and to Andrew). This is the connective tissue
that keeps getting lost in compaction. Read it before reasoning about
"where notothenia is going" or about the type-system angle. It is the
white paper the project doesn't have yet; `DESIGN.md` is the build log,
this is the why.*

Last substantive update: 2026-05-29.

---

## 0. One-paragraph orientation

**Notothenia** (repo name; the product face is **Minard-DB**) is database
schema cartography in the Hylograph family. The thesis: *a database
schema is a type system*, so stop at linting/reports is leaving the
interesting work undone — go to **proofs**. The verification backend is
**Alloy** (Daniel Jackson's relational model finder). The whole thing is
deliberately framed as a member of the **QuickCheck family**: state the
invariant, let the machine find the counterexample. The repo is at
`CodeExplorer/minard-db/`; GitHub `afcondon/notothenia`; Marginalia
project **216** (`bravo-uniform-victor-oscar`). "Cod with only one 'd'" —
a tribute to E. F. Codd.

---

## 1. The three lineages (this is the actual contribution)

The novelty is **not** any single capability. It is the *composition* of
three threads that nobody has braided into a developer-facing tool. Frame
public writing around the lineage, not around claims — it reads as
engineering credibility rather than marketing.

1. **Cunha & Pacheco (~2009) — relational schemas ↔ Alloy.** Established
   that relational DB semantics (schemas, constraints, queries) map
   faithfully onto Alloy's relational logic. *The foundation nobody
   productized.* The theory has sat ~15 years while tooling went to
   linters, ORMs, migration runners. **That gap is notothenia's reason to
   exist.** (Attribution recalled from our own design doc; re-verify the
   exact citation before publishing.)

2. **Daniel Jackson — Alloy, "lightweight formal methods," the
   small-scope hypothesis.** The model finder + the philosophy: bounded
   exhaustive checking with concrete counterexamples, *honest about
   scope*, instead of full theorem proving.

3. **The QuickCheck family — Claessen & Hughes → Runciman (SmallCheck) →
   Kerckhove (sydtest/validity).** The *ergonomics* lineage. Claessen-
   Hughes: property → counterexample. SmallCheck: bounded **exhaustive**
   (not random) — structurally the same bet as Alloy, so the kinship is
   real, not analogy. Kerckhove: the **validity partition** (a
   counterexample must be a *valid* input or you debug the wrong thing)
   and shrinking-toward-minimal.

A fourth body of work joins on the **type** side (see §5): **Mark's
rowtype-yoga typed-Postgres libraries** (`yoga-postgres`,
`yoga-sql-types`, `yoga-postgres-om`). When Andrew said "Mark Eibes SQL
work" he meant exactly this — typed access to Postgres. It is the *same*
thread as the type-system fusion, not a separate one.

How each QuickCheck idea is realized in notothenia:
- validity partition → every Alloy counterexample is a *valid* schema
  instance (the `.als` separates validity facts from the property).
- shrinking → **scope minimization**: report the smallest scope at which
  a property still breaks.
- `classify`/`cover` → **coverage probes**.
- fault localization → name the offending FD (or schema-vs-row level for
  migrations), not just "the rows."

---

## 2. What is BUILT (verified state as of 2026-05-29)

Pipeline: **precompute → store → serve → render**. PureScript throughout.
HTTPurple backend on `:3080`, Halogen frontend on `:3081`. Alloy invoked
as `java -jar` child process, output parsed from `receipt.json`.

**Schema AST** (`src/MinardDB/Schema.purs`): runtime ADT —
`Schema { name, tables }`, `Table { name, columns, primaryKey,
foreignKeys, uniqueConstraints, functionalDependencies }`, `PGType`
(incl. `PGDecimal`/`PGBlob` added in 4c), `FKAction`, `FDSource`
(Declared vs Inferred). This is the hub everything derives from.

**Phase 1–2 — structural proofs** (`Properties.purs`, `Alloy/*`,
`Analyze.purs`, frontend topology + explainers):
- BCNF with per-FD fault localization; FK-acyclicity (`NoFKCycle`).
- validity partition, scope minimization, coverage probes.
- Topology view (sacrificial SVG; layered Kahn layout; cycle/BCNF
  highlights). Explainers for non-DBAs (BCNF, cycles, missing FKs).
- *Caveat: Phase 2 internals described from commit history, not
  re-audited this session. Re-read the `.als` generator before strong
  claims about partition rigor.*

**Phase 3 — migration verification** (`src/MinardDB/Migration*.purs`):
- `Migration` ADT + `applyMigration` + `runSequence` (trace of
  before/after).
- `Migration/Safety.purs`: static RI walker; per-step introduced/
  resolved/standing issues.
- `Migration/Alloy.purs`: **Alloy 6 temporal** model. Tables/columns/FKs
  that ever appear become static sigs; `var sig Active{Table,Column,FK,
  Row}` track membership; one transition predicate per migration kind;
  trace fact chains them with nested `after`s. Two assertions split for
  fault localization: `SchemaRIPreserved` (FK targets present table/col)
  and `RowRIPreserved` (rows' fkTarget present). `check … for 5 but
  1..(2*nSteps+1) steps` (the bound derives from trace length — was a
  hardcoded 20 that silently truncated long histories; fixed in the
  field test).
- **Key 3c insight**: `populate` commits a row's fkTarget on `some
  fkVia` (a row-shape commitment), NOT on `fkVia in ActiveFK`. The
  weaker antecedent let Alloy preload rows whose FK wasn't yet active,
  then a later `addFK` retroactively broke RI on the *populate* step — a
  spurious counterexample. A row referencing another doesn't forget it
  when the FK constraint is dropped.
- `Migration/SQL.purs`: SQL **DDL** parser → `MigrationSequence`. Also
  `schemaFromSql` (replay DDL onto empty schema, *tolerantly* — skips
  steps that error, because real dumps are idempotent) and `dbmateUp`
  (slice `-- migrate:up` section).
- Phase 3e timeline UI (`Frontend/Timeline.purs`): vertical rail,
  per-step standing-issue dots, "broken in transit" callout.
- **Verdict matrix** (the regression invariant): safe → both PROVEN;
  unsafe-t (DROP TABLE) → both BROKEN; unsafe-c (DROP COLUMN) → schema
  BROKEN / rows PROVEN.

**Phase 4 — query reach** (`src/MinardDB/Query*.purs`):
- `Query.purs`: SQL **query** parser. Tokenizes, then *heuristically*
  harvests FROM/JOIN tables (with aliases) + column refs (qualified
  `t.c`, `t.*`, bare `c`, `*`). No expression grammar; errs toward
  *over*-collecting (safe direction for dead-column work).
- `Query/Reach.purs`: resolves refs against a `Schema` → `(table,column)`
  pairs via SQL name resolution (alias→table, `*` expansion, bare-column
  unambiguity rule). Aggregate → dead columns.
- `Query/Report.purs` + `Frontend/Reach.purs`: schema **usage heatmap**
  grid (per-column reach-count bars, dead columns struck through). Served
  at `/api/reach`.
- `SQL/Lexer.purs`: token layer **shared** by the DDL and query parsers
  (extracted in 4a; both parsers ride it).

**Two field tests (the credibility anchors):**
- *registry-dev* (`Migration/SQL/FieldTest.purs`): the PureScript
  Registry's real dbmate migrations. Found a **transient dangling FK**
  (DROP jobs while logs still FKs it) — endpoints clean, RI violated *in
  transit*; the temporal check catches what endpoint-diffing misses. Also
  caught a real model bug (createTable wasn't activating inline FKs).
- *Marginalia* (`Query/FieldTest.purs`): parsed Marginalia's real
  `schema.sql` (14 tables, 120 cols) and harvested SQL literals from its
  PureScript server source. **26 dead columns**, dominated by
  `agent_sessions` and `project_issues` — verified real: the server only
  `DELETE … WHERE project_id`s them (cascade); they're written/read by
  other tooling. Honest caveat: harvester under-reports dynamically-
  concatenated WHERE fragments → "dead" = unreferenced-by-harvested-set,
  a candidate not a verdict.

Frontend has three tabs (Analyses | Migrations | Query reach), hash
routing (`#<int>`, `#m`, `#m/<name>`, `#r`, `#r/<name>`). Reports served
from `reports/` (migration reports unprefixed, reach reports `reach-*`).
A preliminary landing site is at `site/` (deploy target
`notothenia.minard.app`, NOT yet published — pending diagram decisions).

---

## 3. What is NOT built — and it is essentially the TYPE half

The relational-model-finding axis (Cunha-Pacheco × QuickCheck ergonomics ×
real SQL) is substantially realized. The **type-theoretic** axis is a
promissory note. "A schema is a type system" is currently doing
rhetorical work, not engineering work:

- **`PGType` is nominal** — we never reason about domains/subtyping. The
  "type widening is safe" property is catalogued, unimplemented.
- **Reach resolves names, not types** — never checks a WHERE comparison or
  JOIN is type-compatible.
- **No Schema-AST → typed-bindings generator** (the yoga codegen). This
  is the single biggest unbuilt piece and the most direct realization of
  the thesis.
- Others: DuckDB only (no PG); DDL parsing only (no live catalog
  introspection); FD inference designed (Declared vs Inferred) but
  unbuilt; lossless-join / dependency-preservation catalogued, unbuilt.

The next major movement is closing the type half — and §5/§6 show the
target already exists.

---

## 4. The two solvers → a verification *stack*

Andrew's observation: notothenia effectively has two SAT-ish solvers —
Alloy, and PureScript's type inference. The sharper framing is a **stack
of decision procedures**, each layer deciding what the cheaper layer
below cannot express:

1. **HM unification** — types line up (decidable, ~linear).
2. **Type-class resolution + fundeps** — column existence, result-row
   derivation, PK/FK structure (search — *this* is the "second solver").
3. **Typestate via row presence** — clause-ordering protocol (the `stage`
   row in `Q`; see §5).
4. **Bounded model finding (Alloy)** — relational + temporal invariants
   no type system reaches.

Layers 1–3: unbounded, exact, every compile — but only for
type-expressible properties. Layer 4: bounded scope, on-demand — but
expresses the genuinely relational/temporal. **Notothenia's contribution
is layer 4 plus the bridge that lets layers 1–3 enforce layer 4's
findings.**

---

## 5. The rowtype-yoga synthesis (the type half already exists)

Cloned for reference at `GitHub/local-copies/purescript-yoga-postgres`
and `…/purescript-yoga-sql-types` (also `…/purescript-yoga-postgres-om`,
`…/purescript-yoga-om-layer`). Author: **Mark** (rowtype-yoga org).

Two artifacts matter:

**`Table name cols` IS a type-level Schema AST.** From
`yoga-postgres/src/Yoga/Postgres/Schema.purs`:
```purescript
type UsersTable = Table "users"
  ( id    :: PrimaryKey (AutoIncrement Int)
  , email :: Unique String
  , active:: Default "true" Boolean
  , role  :: String )
```
Constraint wrappers: `PrimaryKey`, `AutoIncrement`, `Unique`, `Default s`,
`DefaultExpr s`, `ForeignKey table References col a`, `Nullable`. This
carries the *same information* as notothenia's runtime `Schema` —
including FK referenced table+column — only lifted to the type level.
notothenia's `Schema` is a *value* (fed to Alloy); yoga's `Table` is a
*type* (fed to the compiler). **Two encodings of one object.**

**`Q tables result params stage` is a reach-exact typed query.**
```purescript
newtype Q tables result params stage = Q { sql :: String, values :: Array PG.PGValue }
```
A typed SQL AST that *erases to a string*. The four phantom rows are
compile-time bookkeeping:
- `from (Table name cols)` seeds `tables` (a row-of-rows = FROM context).
- `select @"name, email"` runs `ParseSelect sym tables -> result`
  (fundep): parses the type-level string, looks each column up in
  `tables` (wrong column = type error), **computes** the result row.
- `where_ @"role = $role"` runs `ParseWhere`, harvesting `$role` into
  `params` with type pulled from the column.
- `orderBy/groupBy/having/limit/offset` gated by `HasClause "select"
  stage` + `Row.Lacks … stage` + `Row.Cons … stage stage'` — the `stage`
  row is a **typestate machine** (no double orderBy, no where after
  limit, no having without groupBy).
- `runQuery` uses `ParamsToArray` to positionalize `$name → $1..$n`.

**The punchline: a `Q` value carries its own reach in its type** —
`Q tables result params _` is literally (tables read, columns produced,
params consumed). The exact, no-heuristics version of what Phase 4
reconstructs by harvesting strings. The type checker also already does FK-
driven auto-joins (`FindForeignKeyTo`/`ExtractForeignKeyCol`) and
required-vs-optional insert columns (`InsertableColumnsRL` +
`RequiredColumnsRL` + `Union`).

**Division of labor — they decide different fragments:**

| property | yoga type checker | Alloy |
|---|---|---|
| column exists / type-compatible | ✅ exact, every compile | (wastefully) |
| required-vs-optional insert cols | ✅ | — |
| PK/FK structure of one query | ✅ | — |
| FK graph **acyclicity** | ✗ (uses FKs; never checks the graph) | ✅ |
| BCNF / 3NF / lossless join | ✗ (not type-expressible) | ✅ |
| RI across a migration **trace** | ✗ | ✅ (temporal) |

yoga = local per-query conformance; Alloy = global + temporal invariants.
Neither subsumes the other; the boundary is exactly HM+classes'
expressiveness.

---

## 6. The extension ladder (candidate next work)

Each rung is concrete. The Schema AST is the pivot between value-world
(notothenia/Alloy) and type-world (yoga).

1. **The bridge (codegen, both ways).** notothenia `Schema` → emit
   `Table name (…)` declarations (app gets typed queries). And parse
   `Table` declarations → `Schema` → diff vs live catalog (drift
   detection). *Prerequisite for everything below.* Will expose fidelity
   gaps in our own `Schema` (we discard DEFAULT-expr contents; FK
   refColumns sometimes empty).
   **STATUS (2026-05-29): forward half DONE** — `MinardDB.Codegen.YogaTable`
   (`emitTable`/`emitModule`), dependency-free text gen. Validated against
   *real* yoga: all 14 Marginalia tables → `generated/MarginaliaSchema.purs`
   compiles against `yoga-postgres`; a typed query against the generated
   `ProjectsTable` type-checks, a bogus column fails to compile. The
   reverse half (parse `Table` → `Schema` → diff) is 5b, not yet built.
   Fidelity gaps confirmed exactly as predicted (literal-vs-expression
   defaults recovered heuristically; composite uniques dropped; bigint/
   decimal lossy).
   **STATUS (2026-05-29): reverse half also DONE (5b).**
   `MinardDB.Codegen.YogaParse` parses `Table` decls → `Schema`;
   `MinardDB.Schema.Diff` does a structural drift diff (with
   `pgTypeEquiv` ignoring the lossy collapses + default-text). Round-trip
   on Marginalia = 0 drift; a stale binding caught with exactly the
   injected drifts. The round-trip found a real forward-gen bug
   (Nullable suppressed on defaulted columns — nullable and Default are
   orthogonal), now fixed. **Rung 1 is complete.** Drift detection is a
   real feature now: parse an app's hand-written `Table` bindings, diff
   against the introspected catalog, get the disagreements.

2. **Alloy proves, on the yoga types, what types can't say.** Derive a
   `Schema` from someone's `Table` declarations, run the proof catalog —
   BCNF, FK-acyclicity, lossless join, migration-RI. Their typed bindings
   gain the global proofs the type system structurally can't express.

3. **Reach + dead columns as a *compile-time* computation.** Collect the
   app's queries into a type-visible **query manifest** (record of `Q`s);
   a type class folds `result ∪ params ∪ where-cols` across it and
   subtracts from the schema → the dead set, exactly, at compile time.
   "Your compiler tells you which columns are dead." Phase 4's harvester
   becomes the *fallback for raw/legacy SQL*; for yoga queries it's
   replaced by something exact. Two ends of one spectrum.

4. **"A migration breaks a query I run" → a type error.** A migration is
   `TableV1 → TableV2`. If the manifest is typed against the schema and
   V2 drops/retypes a reached column, **the manifest stops compiling, at
   the exact call site.** Turns the dead-column → DROP COLUMN → verify
   loop from on-demand into *continuous* (every build). Alloy still owns
   the deep relational/temporal half.

5. **The two solvers cooperate, both directions.** Alloy finds a BCNF
   violation → notothenia emits the *decomposed* `Table` types → the
   denormalized access path is now ill-typed (Alloy diagnoses, types
   enforce the cure). Reverse: exact reach *prunes Alloy's scope* (verify
   over what the app actually touches).

**Honest constraints:** rungs 3–4 cover only the statically-constructed
`Q` subset; dynamic SQL falls back to heuristic reach ("exact for typed
queries, approximate for the rest"). Type-level folding over a big
manifest has real compiler cost. The bridge forces tightening of our
Schema fidelity.

Andrew's instinct (2026-05-29): rungs **3 + 4** are most novel/demoable
(literal fusion of Phases 3–4 with Mark's machinery), but rung **1** is
the prerequisite to start cutting. Data-visualization layer is wanted
later; Andrew hopes to contribute an original viz insight once he
understands the whole problem space (current SVG views are explicitly
sacrificial, pending a Hylograph rewrite).

---

## 7. Provenance & pointers

- Repo: `CodeExplorer/minard-db/` · GitHub `afcondon/notothenia` ·
  Marginalia **216**.
- Design log: `DESIGN.md` (per-phase as-built notes near the top).
- yoga refs (cloned 2026-05-29):
  `GitHub/local-copies/purescript-yoga-postgres` (has `Schema.purs`,
  3588 lines — `Q`, the combinators, the type-level classes),
  `…/purescript-yoga-sql-types`, `…/purescript-yoga-postgres-om`
  (Om wrapper + `ClientOm.purs` CRUD combinators, ~1850 lines).
- Field-test subjects:
  `GitHub/local-copies/registry-dev/db/migrations/` (dbmate),
  `agent-teams/project-tracker/` (Marginalia: `database/schema.sql`,
  `server/src/**/*.purs`). Marginalia API now on the MacMini via
  Tailscale: `http://andrews-mac-mini:3100` (NOT localhost).
- `yoga-postgres` = `rowtype-yoga` org, author Mark.
- Citations to firm up before publishing: Cunha & Pacheco (2009, Alloy↔
  relational DB); Jackson (Alloy / *Software Abstractions* / small-scope);
  Claessen & Hughes (QuickCheck, 2000); Runciman et al. (SmallCheck,
  2008); Kerckhove (validity/genvalidity/sydtest); Codd (relational
  model, normal forms); for the "fuse a solver into the type checker"
  prior art — Liquid Haskell / F* (refinement types + SMT), the
  neighbouring design space.

## 8. Framing guidance for any public writing

Lead with the **lineage**, not claims: Codd → Cunha-Pacheco → Jackson →
the QuickCheck family → (the type half) Mark's rowtype-yoga. Say what each
contributed and what was missing; frame notothenia as the composition;
state the type-system fusion as *declared future work*, not implied
present capability. The honest version is the more credible one. The
landing copy in `site/` currently over-indexes on claims — rewrite in the
lineage register when the diagrams are settled.
