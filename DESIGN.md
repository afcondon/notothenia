# Notothenia / Minard-DB Design

> **Provenance.** This document is the versioned home of the design
> sketch that was originally developed in a planning session
> (`~/.claude/plans/idempotent-whistling-crayon.md`). Future updates
> belong here, in the repo, not in that ephemeral file.

## Status (2026-05-28)

**Phase 0 (scaffold) — done.** Schema AST, Alloy compiler, Alloy
invoker, Receipt parser. Smoke test passes (`minard-smoke`,
`minard-smoke-bad`).

**Phase 1 (real-world analysis + storage + UI) — done.** Catalog
introspection (`tools/introspect-duckdb.py`), self-contained DuckDB
storage (`database/notothenia.duckdb`), HTTPurple backend on :3080,
Halogen frontend on :3081, non-DBA explainer sections. Two real
analyses captured:

- **Minard's `ce-unified.duckdb`** — declares FKs; `NoFKCycle` is SAT
  with witness `module_namespaces$7` (the `parent_id` self-reference
  permits cycles, though the loader can't actually produce one).
- **Marginalia's `tracker.duckdb`** — zero declared FKs, fifteen
  inferred from naming convention; `projects.parent_id`
  self-reference cycle hypothetically reachable.

**Phase 2 — in progress.** Now framed as the QuickCheck-family
member it actually is.

- **2a (validity partition + BCNF + fault-localization)** — done.
  Generated `.als` is split into VALIDITY FACTS / PROPERTIES UNDER
  TEST sections; BCNF emits one `assert`/`check` per declared FD so
  the witness names the offending dependency.
- **2b (scope minimization — the shrinking analogue)** — done.
  Binary search downward from initialScope; the smallest
  counterexample-producing scope is persisted as `min_scope` and
  rendered as `N → m` in the frontend, matching QC shrinker
  notation.
- **2c (coverage probes — the `classify` analogue)** — done.
  Three `run` commands per schema (`AcyclicShape`, `CyclicShape`,
  `ChainOfLength2`) report whether each FK-graph shape is
  realizable within scope. On a structurally-safe schema like the
  TREE smoke fixture, `CyclicShape UNSAT` corroborates `NoFKCycle`
  PROVEN — the proof has bite because the constraints actively
  rule out cycles, not just "we didn't happen to find one".
- **2d (topology view)** — sacrificial SVG MVP done. Halogen
  component (`MinardDB.Frontend.Topology`) renders tables as
  rounded rects in a topologically-sorted layered layout, edges
  as Bézier curves, cycle witnesses with red borders, self-
  loops as small arcs, BCNF violators with orange fill. Schema
  data flows through a re-read of the original JSON source on
  the GetAnalysis endpoint; degrades gracefully if the source
  file is gone. Hash-deep-link (`/#<id>`) added in passing for
  reload-survival and shareable URLs. The MVP exposes layout
  problems a force-directed Hylograph version would solve:
  layer 0 collects every source table into one long row,
  hubs aren't drawn as hubs, and the self-loop ornament is too
  small to read as "cycle of length 1". Topology view is
  retained as a learning artifact until the Hylograph-backed
  rewrite lands.

**Phase 3 — in progress (migration verification).**

- **3a (groundwork: migration model + static safety)** — done.
  `MinardDB.Migration` defines the `Migration` ADT (CreateTable,
  DropTable, AddColumn, DropColumn, AddForeignKey,
  DropForeignKey) and `applyMigration :: Migration -> Schema ->
  Either String Schema`. `runSequence` accumulates a trace of
  `{ migration, before, after }`. `MinardDB.Migration.Safety`
  scans each step for `MissingTargetTable` / `MissingTargetColumn`
  issues and reports them as *introduced* / *resolved* /
  *standing* per step. Smoke test (`MinardDB.Migration.Smoke`)
  covers a safe build-up plus two unsafe variants
  (DROP TABLE breaks the FK at table level; DROP COLUMN breaks
  it at column level). All three sequences produce the expected
  output, distinguishing missing-table from missing-column
  dangling FKs.
- **3b (Alloy 6 temporal model)** — done.
  `MinardDB.Migration.Alloy.generateTemporal` emits an Alloy 6
  model where every table/column/FK that *ever* appears across
  the trace becomes a static one-sig, and `var sig
  Active{Table,Column,FK}` track current membership. One
  transition predicate per migration kind, plus a `stutter`
  for time after the final step. The trace fact chains the
  migrations with nested `after`s; the tail `after^n always
  stutter` pins the trace stable. The assertion is `always
  RIHolds` (every active FK targets an active table + active
  columns); `check RIPreserved for 5 but 1..15 steps`. The
  smoke test runs both passes on each fixture and reports
  them side-by-side. All three fixtures agree: SAFE → Alloy
  UNSAT (PROVEN), UNSAFE-T and UNSAFE-C → Alloy SAT
  (BROKEN). The cross-check is the deliverable: when both
  the deterministic walker and bounded model finder agree,
  we have meaningful confidence; if they ever disagree, the
  disagreement is itself a useful signal.
- **3c (row-level model + fault localization)** — done.
  Generated model adds a `Row` sig with `ofTable`, `lone fkVia`,
  `lone fkTarget` (plus consistency facts pinning the relations
  to the schema), a `var sig ActiveRow`, a `populate` transition
  that lets Alloy introduce row populations between migrations,
  and cascade semantics on `dropTable`. The assertion splits in
  two: `SchemaRIPreserved` (FK targets a present table/column)
  and `RowRIPreserved` (row's fkTarget is in ActiveRow when
  fkVia is set). Verdicts:

    SAFE     → both PROVEN
    UNSAFE-T → both BROKEN
    UNSAFE-C → schema BROKEN, rows PROVEN

  The split is the fault-localization payoff. Row-level PROVEN
  on a schema-BROKEN sequence says "the data is fine; this is a
  column-rewire problem, not a data-loss event". Row-level
  BROKEN on a schema-BROKEN sequence says "the cascade will
  orphan rows — fix-up needs migration logic, not just DDL".

  The non-obvious part was getting `populate` to commit rows to
  their fkTarget *unconditionally* on `some fkVia` rather than
  conditionally on `fkVia in ActiveFK`. The weaker antecedent
  let Alloy pre-load rows whose constraint wasn't currently
  enforced, then a later `addFK` retroactively breaks RI on the
  *populate* step rather than on a subsequent destructive
  migration — a spurious counterexample. Treating fkVia as a
  row-shape commitment that survives FK-toggling matches the
  real-DB intuition (a row referencing another row doesn't
  forget about it just because the FK constraint is dropped).
- **3d (SQL parsing)** — defer. Hand-coded migrations are
  sufficient to nail the semantics; a `parseSql` pass is
  mechanical and lands once 3a/3b/3c are stable.
- **3e (migration timeline UI)** — defer until the safety
  pass has a real use case in the wild.

**Note: Data Model section below has drifted.** The plan originally
described a shared `minard_db_*` namespace inside Minard's DuckDB.
Implementation diverged: notothenia owns `database/notothenia.duckdb`
with tables `analyses`, `analysis_inferred_fks`, `analysis_proofs`
(schema at `database/schema.sql`). The separate-database choice was
deliberate (avoids competing for Minard's exclusive lock during
analysis). Update this section when convenient; meantime the
authoritative schema lives in the SQL file.

---

## Context

Minard-PS understands PureScript codebases at full depth: modules,
declarations, type ASTs, coupling metrics. The compiler artifacts
(docs.json, corefn.json) are the source of truth, and the visualization
layer makes structural properties visible.

Minard-DB applies the same philosophy to databases. The key insight:
**a database schema IS a type system** — normal forms are type safety
levels, FKs are type-level references, denormalization is unsafeCoerce,
and migrations are type-system evolution. We can go beyond reports and
warnings toward **proofs of correctness** using Alloy as a formal
verification backend.

No production tool does this today. The theoretical foundation is solid
(Cunha & Pacheco proved Alloy↔DB semantic equivalence in 2009), but
nobody has built the developer-facing product.

## Architecture: Schema AST as Hub

```
                    ┌─────────────┐
  DDL / pg_catalog  │  Schema AST │  yoga-postgres
  DuckDB catalog ──►│  (PureScript │──► typed query
  .sql files        │   ADT)      │    bindings
                    └──────┬──────┘
                           │
              ┌────────────┼────────────┐
              ▼            ▼            ▼
         Alloy .als    DuckDB       Hylograph
         (generate,    (store for   (visualize)
          verify,      Minard-DB
          parse XML)   tables)
```

Everything derives from the **Schema AST** — a PureScript ADT that
represents a database schema with full fidelity:

```purescript
type Schema =
  { name :: String
  , tables :: Array Table
  , views :: Array View
  , functions :: Array StoredFunction
  }

type Table =
  { name :: String
  , schema :: String            -- e.g. "public"
  , columns :: Array Column
  , primaryKey :: Array String  -- column names
  , foreignKeys :: Array ForeignKey
  , uniqueConstraints :: Array UniqueConstraint
  , checkConstraints :: Array CheckConstraint
  , indexes :: Array Index
  }

type Column =
  { name :: String
  , dataType :: PGType          -- ADT: PGInt | PGText | PGTimestamp | ...
  , nullable :: Boolean
  , defaultExpr :: Maybe String
  }

type ForeignKey =
  { columns :: Array String
  , refTable :: String
  , refColumns :: Array String
  , onDelete :: FKAction        -- Cascade | SetNull | Restrict | NoAction
  , onUpdate :: FKAction
  }

type FunctionalDependency =
  { determinant :: Set String   -- LHS columns
  , dependent :: Set String     -- RHS columns
  , source :: FDSource          -- Declared (from constraint) | Inferred (from data)
  }
```

### Ingestion paths (how the AST gets populated)

1. **DDL parsing**: parse CREATE TABLE / ALTER TABLE SQL into the AST.
   Handles the common case of schema-as-code (migration files).

2. **Catalog introspection**: query pg_catalog / information_schema /
   DuckDB's information_schema directly. Handles the "point at a live
   database" case.

3. **Manual declaration** (yoga-postgres style): PureScript phantom
   types that ARE the schema. The AST is derived from the types at
   compile time.

## Alloy Integration

### How it works

1. **Generate**: Schema AST → Alloy model (.als file). Each table
   becomes a `sig`, FKs become relational fields, constraints become
   `fact` blocks.

2. **Verify**: Spawn `java -jar alloy.jar` as a child process with
   the .als file. Alloy's SAT solver either proves the property holds
   (within scope) or produces a concrete counterexample.

3. **Parse**: Read Alloy's XML output. Extract the counterexample
   (specific table rows that violate the property) or the "no
   counterexample found" result.

4. **Display**: Visualize the result — either a green checkmark with
   the proof scope, or the counterexample rendered as actual table
   rows in the Minard-DB UI.

### What we'd verify (the proof catalog)

**Schema-level proofs:**

| Property | Alloy encoding | What a counterexample looks like |
|----------|---------------|----------------------------------|
| BCNF compliance | For every non-trivial FD X→Y, X is a superkey | "Column `city` depends on `zip_code`, but `zip_code` is not a key" |
| 3NF compliance | Every non-trivial FD either has a superkey determinant or the dependent is part of a candidate key | Specific FD + the candidate keys it violates |
| Lossless-join decomposition | Natural join of projections equals original | Two rows that produce a spurious tuple after join |
| Dependency preservation | Every FD is enforceable on at least one decomposed table | The FD that can't be checked without rejoining |
| FK acyclicity | No cycle in the FK graph | The cycle path: A→B→C→A |
| Redundant constraint | Removing constraint X, all other constraints still hold | (No counterexample = constraint is redundant) |

**Migration-level proofs (Alloy 6 temporal logic):**

| Property | Alloy encoding | What a counterexample looks like |
|----------|---------------|----------------------------------|
| Referential integrity preserved | `always (refIntegrity implies after refIntegrity)` | The migration step that creates a dangling FK |
| No data loss | Before/after schema join reproduces all original rows | The row that disappears after ALTER |
| Column type widening is safe | New type subsumes old type's domain | The value that doesn't fit the new type |

**Query-level proofs:**

| Property | Alloy encoding | What a counterexample looks like |
|----------|---------------|----------------------------------|
| Query equivalence | Two predicates produce identical result sets | The database state where they diverge |
| View materialization correctness | Materialized view = base query | The state where they're out of sync |

### Scope and confidence

Alloy checks within a bounded scope (number of rows per table).
Scope 5-8 catches most structural bugs (the "small scope hypothesis").
We'd report results as: "Verified: BCNF holds for all instances with
≤8 rows per table. No counterexample found." — honest about what
was checked.

## Visualization (Hylograph)

Following Minard-PS patterns but adapted for schemas:

**Primary view: Schema topology**
- Nodes = tables, sized by column count or row estimate
- Edges = foreign keys, colored by ON DELETE action
- Clusters = schemas (public, auth, etc.)
- Node color = normal form level (BCNF=green, 3NF=yellow, 2NF=orange, 1NF=red)
- Violation edges highlighted (cycles, dangling FKs)

**Query reach map** (from the earlier brainstorm)
- Given a query, highlight which tables and columns it touches
- Sankey flow: data path through joins/filters
- Dead column detection: columns no query ever references

**Migration timeline**
- Horizontal axis = migration sequence
- Tables appear/disappear/change over time
- Alloy proof results annotated at each step

**Dependency matrix**
- Tables on both axes
- Cell = FK relationship (directional)
- Highlights cycles and clusters

## Data Model (DuckDB) — STALE; see Status

> Original plan: shared CodeExplorer DuckDB, namespaced with
> `minard_db_` prefix alongside Minard-PS's tables. **Implementation
> diverged** to a separate `database/notothenia.duckdb` with simpler
> tables (`analyses`, `analysis_inferred_fks`, `analysis_proofs`).
> See `database/schema.sql` for the canonical current shape. The
> original ambitious schema below remains as the longer-term target.

```
minard_db_projects
  ├── id, name, dsn, description, created_at
  └── minard_db_snapshots
      ├── id, project_id, git_hash, captured_at
      └── minard_db_tables
          ├── id, snapshot_id, schema_name, table_name, estimated_rows
          ├── minard_db_columns
          │   └── id, table_id, name, data_type, nullable, default_expr, ordinal
          ├── minard_db_constraints
          │   └── id, table_id, kind (pk/fk/unique/check), columns, ref_table, ref_columns
          ├── minard_db_indexes
          │   └── id, table_id, columns, is_unique, method (btree/hash/gin/gist)
          └── minard_db_functional_deps
              └── id, table_id, determinant, dependent, source (declared/inferred)

minard_db_queries (optional, for reach analysis)
  └── id, snapshot_id, sql_text, source_file, tables_referenced, columns_referenced

minard_db_proofs
  └── id, snapshot_id, property, scope, result (verified/counterexample), 
      counterexample_xml, checked_at
```

## Technology Stack

| Layer | Technology | Rationale |
|-------|-----------|-----------|
| Schema AST | PureScript ADT | Type-safe, composable, shared with yoga-postgres |
| DDL parser | PureScript (purescript-parsing) or Python | PureScript preferred for type integration |
| Catalog introspection | SQL queries via existing DuckDB/PG FFI | Already have DuckDB FFI in Minard |
| Alloy verification | Java CLI (child process) | Clean separation; no JVM in build chain |
| Alloy model generation | PureScript (Schema AST → .als text) | Direct, type-safe generation |
| Alloy result parsing | PureScript XML parser | Parse counterexample XML |
| Storage | DuckDB (notothenia.duckdb, separate from Minard) | Avoids exclusive-lock contention |
| Visualization | PureScript + Halogen + Hylograph | Existing infrastructure |
| Query bindings | yoga-postgres patterns | Compile-time query safety |

## MVP Scope

**Phase 1: Schema ingestion + topology visualization** — DONE
- Parse DDL (CREATE TABLE) into Schema AST
- Store in DuckDB
- Render schema topology graph (tables + FKs) with Hylograph
- Test case: Marginalia's DuckDB schema, Minard's own schema

**Phase 2: Normal form proofs, framed as the QuickCheck-family member it is**

Notothenia is a property-based checker. Not "loosely analogous" —
genuinely the same workflow:

> State the invariant. Let the machine find the counterexample.

Alloy is to schemas what SmallCheck is to Haskell values: bounded
*exhaustive* enumeration via SAT rather than random sampling, with
minimal-by-construction counterexamples (Daniel Jackson's "small scope
hypothesis" is the same empirical bet as SmallCheck's "small inputs
surface bugs"). Phase 1 already produces counterexamples; Phase 2 is
about importing the accumulated ergonomics the QuickCheck family has
built up around that core workflow.

### Lessons borrowed

**1. Validity partition (from Tom Sydney Kerckhove's `validity` /
`genvalidity` / `sydtest`).** The classic QC shrink failure mode:
shrinker minimizes past validity, producing a degenerate witness that
"fails" the property but actually fails for an unrelated reason — you
debug the wrong thing. Kerckhove's fix is a `Validity` typeclass that
defines what a meaningful input looks like, and a shrink function
derived from that definition so it can't drift.

The schema analogue: partition our generated Alloy file into two
clearly labeled sections:

```als
// ─── VALIDITY FACTS (always on) ─────────────────────────
// What makes something a meaningful schema instance:
// PKs are unique, FK targets exist, type domains are respected,
// declared UNIQUEs hold, NOT NULL is honored.
fact ValidSchema { ... }

// ─── PROPERTY UNDER TEST ────────────────────────────────
assert BCNF { ... }
check BCNF for 5
```

Then every counterexample is **guaranteed to be a valid schema
instance** that violates *only* the property under test. The witness
for "BCNF fails" can't be "two rows with the same PK" or "a dangling
FK" — those are excluded by validity. This is the right structural
shape regardless of borrowing the framing, and the framing makes us
maintain the partition rigorously.

**2. Scope minimization (shrinking analogue).** When Alloy finds a
counterexample at scope 5, automatically re-check at scope 4, 3, 2,
1, and report the *minimum* scope at which the property breaks. A
2-row witness is dramatically more debuggable than an 8-row one.
This is exactly what QC's shrinking delivers — we get it for free
because Alloy is already enumerating within scope, we just iterate.

UX: "BCNF first breaks at scope 3 with witness {row₁=…, row₂=…,
row₃=…}; the offending FD is `zip → city`."

**3. Property catalog (`quickcheck-classes` analogue).** Pre-built
properties as importable functions:

```purescript
module MinardDB.Properties where

bcnf            :: Schema -> AlloyCommand
threeNF         :: Schema -> AlloyCommand
noFKCycle       :: Schema -> AlloyCommand
losslessJoin    :: Schema -> AlloyCommand
fkTargetsExist  :: Schema -> AlloyCommand
uniqueIsUnique  :: Schema -> AlloyCommand   -- declared UNIQUEs really are unique
notNullHolds    :: Schema -> AlloyCommand
```

Each emits the `assert`/`check` block; the validity facts are emitted
once by the Schema → Alloy compiler.

**4. `classify` / `cover` analogue.** QC's `classify` reports the
distribution of input shapes that exercised the property. Translate:
run constrained `run` commands alongside the main `check` —

```als
run TreeShape { isTree[FKs] } for 5      -- did we cover tree-shaped FK graphs?
run ForestShape { isForest[FKs] } for 5
run CyclicShape { some cycles[FKs] } for 5
```

Notothenia reports: "checked at scope 5; covered 17 tree-shaped
instances, 5 forests, 3 cyclic; BCNF violated in 2 of the cyclic ones."
This is the coverage claim Phase 1 doesn't make.

**5. Fault localization.** QC counterexamples are blobs; better
property-test frameworks identify *which sub-structure* causes the
failure. For BCNF the analogue is specific — the witness identifies
*the offending FD*, not just "the rows". Implementation: for each
declared/inferred FD, emit its own assertion; the first to fail
identifies the offender.

**6. Familiar API shape.** Keep the surface QC-shaped so the
intuition transfers:

```purescript
import MinardDB.Check (check, Result(..))
import MinardDB.Properties (bcnf, noFKCycle)

result <- check schema bcnf
case result of
  Holds { scope }      -> ...   -- "no counterexample at scope ≤ N"
  Fails { minScope, witness, locus } -> ...
```

`Holds` / `Fails` mirrors QC's `Success` / `Failure`. `minScope` is
the shrinking output. `locus` is the fault-localized sub-structure.

### Deliverables

- `MinardDB.Properties` module with the property catalog above
- `Schema → Alloy` compiler refactored to emit the validity/property
  partition explicitly
- Scope minimization loop in `MinardDB.Alloy.Invoke`
- Fault localization for BCNF/3NF (per-FD assertions)
- Coverage reporter (constrained `run` commands)
- Topology view: node colour = normal-form level (BCNF/3NF/2NF/1NF),
  violation edges highlighted, fault-localized FD called out

**Phase 3: Migration verification**
- Parse migration sequences (ordered .sql files)
- Generate temporal Alloy model
- Verify referential integrity preservation across migrations
- Migration timeline visualization

**Phase 4: Query reach analysis**
- Parse SQL queries (from application code or query logs)
- Map queries to tables/columns touched
- Reach map visualization
- Dead column detection

## Test Cases

The ecosystem provides excellent test subjects:

1. **Marginalia's DuckDB** — project tracker schema with FKs, status
   lifecycle, tags, servers, attachments. Known to be well-structured.

2. **Minard's own DuckDB** — complex schema with snapshot chains,
   package versions, type ASTs. Good stress test.

3. **A deliberately denormalized schema** — create a test fixture
   with known normal form violations to verify Alloy catches them.

## Design Decisions (with positions)

### 1. FD inference from data — yes, but kept separate

There's a tension here: data-mined FDs are powerful (they find the
implicit constraints nobody bothered to declare) but they're
probabilistic, not proofs. A FD that holds across 10M rows could be
broken by row 10M+1.

**Position**: Keep two distinct concepts in the Schema AST:

```purescript
type FunctionalDependency =
  { determinant :: Set String
  , dependent :: Set String
  , source :: FDSource
  }

data FDSource
  = Declared            -- from PK, UNIQUE, FK — Alloy proof material
  | Inferred Stats      -- from data sampling — shown as candidates
```

- **Declared FDs** feed into Alloy proofs. These are statements about
  the schema's intent.
- **Inferred FDs** are surfaced in the UI as "candidates for
  declaration" — observations the user can confirm and promote to
  declared status, which would emit suggested ALTER TABLE statements.

This preserves the proof-vs-evidence distinction. Inference is for
discovery and refactoring suggestions, not for the verification layer.

### 2. yoga-postgres integration — generate from Schema AST

The cleanest path: the Schema AST is the source of truth, and the
yoga-postgres type declarations are generated from it. One AST →
typed bindings + Alloy model + visualization.

**Position**: Build a `Schema AST → yoga-postgres .purs` code
generator as part of Phase 1. The output is checked into the
consuming project alongside hand-written code, with a header
comment marking it as generated.

This gives us a closed loop: the live DB introspection populates the
AST, the AST generates the typed bindings, the bindings give
compile-time query safety in PureScript application code. If the DB
schema drifts from the bindings, regenerating catches it.

Manual yoga-postgres declarations (for the user's own existing code)
should also be cross-checked — parse them back into the AST and
compare to the introspected AST. Disagreements are a real bug.

### 3. QuickCheck-family ergonomics + Kerckhove's validity partition

Notothenia is a member of the QuickCheck family. The deliberate
positioning yields three concrete commitments:

**a. The Schema → Alloy compiler emits two clearly labeled sections:**
structural validity facts (what counts as a meaningful schema instance)
vs. property under test (the one assertion being checked). Borrowed
from Kerckhove's `validity` insight: a counterexample to property P
must be a *valid* input, otherwise you debug the wrong thing. Without
this partition, the BCNF witness might be a schema with a dangling
FK — which is its own bug, not a BCNF violation.

**b. The PureScript API stays QC-shaped.** `check :: Schema -> Property
-> Aff Result`, with `Result = Holds { scope } | Fails { minScope,
witness, locus }`. Properties live in a `MinardDB.Properties` module
the same way `quickcheck-classes` exposes `monoidLaws`, `functorLaws`
etc. Users with QuickCheck intuition find their footing immediately.

**c. Scope minimization plays the role QC's shrinker plays.** When a
counterexample lands at scope N, we iterate downward to find the
smallest scope at which the property breaks. This is the user-facing
deliverable that turns Alloy from "academic prover" into "debuggable
testing tool".

The closest classical analogue is SmallCheck (Colin Runciman) — bounded
exhaustive enumeration, the same "small scope hypothesis" empirical
bet. Daniel Jackson's "lightweight formal methods" framing for Alloy
was published in the same spirit as Hughes-Claessen's "lightweight
verification" framing for QuickCheck. Notothenia inherits both.

### 4. Multi-database support — DuckDB first, designed for PG next

DuckDB and PostgreSQL both implement standard `information_schema`,
which gives us 80% of what we need uniformly. The differences are
mostly around system catalogs and DDL syntax.

**Position**: Phase 1 targets DuckDB exclusively (it's where Minard
already lives, and where our test schemas exist). The introspection
layer is built behind a thin `Backend` typeclass with two methods:
`listTables` and `describeTable`, each returning portions of the
Schema AST. PostgreSQL becomes the second backend in Phase 2 or 3.

The Alloy generation, visualization, and proof catalog are all
backend-agnostic — they consume the Schema AST, which is the same
regardless of source.

## Family reading list

The QuickCheck-family positioning (§3 above) draws from:

- **QuickCheck** (Hughes & Claessen 2000) — the founding paper
- **SmallCheck** (Runciman, Naylor, Lindblad) — bounded exhaustive
  enumeration; closest classical analogue to Alloy
- **Hedgehog** — integrated shrinking; relevant for "no separate
  shrinker"
- **`validity` / `genvalidity` / `sydtest`** (Tom Sydney Kerckhove) —
  validity-based testing; the validity partition idea above is from
  here
- **`quickcheck-classes` / `hedgehog-classes`** — property catalog
  libraries; the model for `MinardDB.Properties`
- **`quickcheck-state-machine`** — state machine testing; the model
  for Phase 3 migration verification
- **Daniel Jackson, *Software Abstractions*** — the Alloy book; the
  "small scope hypothesis" chapter is the family bridge

## Remaining Open Questions

1. **Alloy scope defaults**: What scope gives the best
   confidence/performance tradeoff for typical schemas? Need empirical
   data from our test cases — defer until Phase 2.

2. **Where does the Schema AST live?**: Should it be its own
   PureScript package (`purescript-schema-ast`) for reuse by
   yoga-postgres, ShapedSteer, Rule4, etc.? Or live inside Minard-DB
   until it proves out? Lean toward "own package" but defer the
   extraction until the AST shape stabilizes.
