# Where notothenia sits relative to the database's own enforcement

For several of the problems notothenia detects, the honest answer is
"the database already does this, just later and worse." For others it
is "nothing in the stack does this." Knowing which is which is the
whole justification for the tool.

## The organizing axis: what each enforcer quantifies over, and when

- **The DB engine** quantifies over *the rows that exist right now*
  (extensional), and acts *reactively* at write time. Its verdict is
  "this operation is rejected," and it stays silent until an operation
  arrives.
- **notothenia / Alloy** quantifies over *all possible instances within
  scope* (bounded-universal), and acts *proactively* at design / CI
  time. Its verdict is "no instance can violate this" or "here is the
  smallest instance that does."
- **The PureScript compiler** (the type-world half of the bridge)
  quantifies over *all executions of the code* against the *declared*
  schema, at compile time.

Their blind spots don't overlap: the DB can't see your code or your
design intent; the compiler can't see the live database; neither knows
any normalization theory. notothenia lives in exactly those gaps.

## The spectrum — from "DB fully owns it" to "only notothenia can see it"

| notothenia check | DB's own tool | When the DB acts | What notothenia adds |
|---|---|---|---|
| PK unique, NOT NULL, UNIQUE, FK exists | constraints, enforced via indexes/triggers | runtime, per write, on real rows | These are notothenia's **validity facts**, not its findings — the floor it assumes. It doesn't compete here; it *builds on* them (every counterexample is guaranteed to already satisfy them). |
| CHECK predicates | `CHECK (...)` | runtime, per row | The DB enforces each CHECK independently and locally (no subqueries, no cross-row, no cross-table). notothenia can ask whether a *set* of CHECKs is **mutually contradictory** — the DB never will; it just rejects every insert forever. |
| FK target table/column exists; type-compatible | DDL-time validation | at `ALTER` / `CREATE` | notothenia checks this across a whole **migration sequence** statically. On MySQL (no transactional DDL) that's the difference between catching it in CI and stranding a half-applied migration in prod. On Postgres it saves the failed-deploy round-trip. |
| Referential integrity preserved across a migration | — | — | The DB validates each statement as it runs; it has no notion of "this step *introduces* a dangling FK that step N+3 will trip over." Temporal Alloy proves the invariant holds at *every* intermediate state. |
| BCNF / 3NF compliance | **nothing** | never | The engine has no concept of a functional dependency beyond keys. It will never tell you `zip → city` is a transitive dependency. Pure design quality, invisible to the engine. |
| FK acyclicity | **nothing** | never | Postgres happily builds FK cycles (sometimes legal via deferred constraints, often a smell). Never flagged. |
| Lossless-join / dependency preservation | **nothing** | never | These are properties of a *decomposition* — they presuppose "the schema this was split from," which the DB has no memory of. |
| Redundant constraint | **nothing** | never | The DB enforces every constraint independently; it never notices one is implied by the others. |
| Dead columns | **nothing** | never | The DB doesn't know your query corpus, so it can't know a column is never read. |
| Drift: yoga types ⇄ live schema | **nothing** | never | This is *outside the database*. The DB can't enforce it (the types aren't in the DB); the compiler can't (the live schema isn't in the compiler). The **bridge is the only thing that sees both.** |

## Two observations that fall out of the framing

### 1. The satisfiability dual is a service the DB structurally cannot provide

Everything above is `check` (∀ instances, does P hold?). The flip side
is `run` (∃ a non-empty instance at all?). A schema can be *declarable
but unpopulatable* — mutually-required FKs in a cycle, contradictory
CHECKs, a NOT-NULL self-FK with no nullable escape. The DB will let you
`CREATE` all of it, then reject literally every `INSERT` at runtime and
never explain why, one failed transaction at a time. Alloy answering
"find me one valid instance" tells you *at design time* whether the
schema admits data — and the DB has no equivalent verb.

### 2. notothenia is, almost exactly, `CREATE ASSERTION` delivered as static proof instead of runtime enforcement

SQL-92 specified `CREATE ASSERTION` — arbitrary multi-table boolean
invariants over the whole database. No major engine has ever
implemented it, because enforcing it on every write is too expensive.
That gap is *why* the "never" rows above are empty. notothenia doesn't
enforce those assertions at runtime; it *proves* them at design time
over a bounded universe. It's the feature the standard wanted, repriced
from "check on every transaction forever" to "check once in CI, within
scope."

## The one-line situating

The DB enforces the constraints you declared, reactively, against the
data you have. notothenia checks the constraints you *meant* —
including whether the ones you declared are jointly satisfiable and
sufficient — proactively, against every instance you could have. It is
strictly upstream of the engine, and for the bottom half of that table
it is upstream of *anything* in the stack.
