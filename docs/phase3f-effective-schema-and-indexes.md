# Phase 3f — index referential integrity + effective-schema semantics

**Status:** plan, ready to build. Extends Phase 3 (migration verification);
picks up where 3d (SQL parsing) stopped — it deliberately skipped indexes.
**Grounded in:** notothenia at `7795d2a`. Line/module refs below are to that
commit.
**Motivating bug:** a real boot crash in Minard's server (`minard`, the
sibling repo), recorded in `minard/docs/kb/research/markgraf-static-analysis-lessons.md`
as "lesson #1". The first external codebase (markgraf) surfaced it; this is
the work that would let notothenia catch it.

## The bug we want to catch

Minard's server ran this annotations migration (one batch, then a second):

```sql
CREATE TABLE IF NOT EXISTS annotations (... project_id INTEGER ...);
CREATE INDEX  IF NOT EXISTS idx_annotations_project ON annotations(project_id);
-- separately, later:
ALTER TABLE annotations ADD COLUMN IF NOT EXISTS project_id INTEGER;
```

On any database where `annotations` already existed *without* `project_id`
(every fresh loader-created DB), the `CREATE TABLE IF NOT EXISTS` is a
**no-op** — so `project_id` is absent when `CREATE INDEX` references it.
Binder error, server crashes on boot. The `ALTER` that would have fixed it
runs too late.

The bug lives entirely in the gap between the **declared** schema
(`project_id` is right there in the CREATE text) and the **effective**
schema (the CREATE no-op'd, so the column isn't there). Catching it is
exactly the kind of thing notothenia exists to do — but three properties of
the current model stop it.

## Why the current model misses it

Verified against source:

1. **Indexes aren't parsed.** `MinardDB.Migration.SQL`'s module header lists
   indexes under "What we do NOT handle (deliberately)". The parser would
   skip or fail on `CREATE INDEX`.

2. **Indexes aren't modelled.** The `Migration` ADT
   (`MinardDB.Migration`, line 46) is `CreateTable | DropTable | AddColumn |
   DropColumn | AddForeignKey | DropForeignKey`. No index constructor.

3. **`IF NOT EXISTS` is parsed-and-discarded, and CreateTable-on-existing is
   an error, not a no-op.** `Migration.SQL` (~line 429) yields a bare
   `CreateTable` — the `IF NOT EXISTS` token is dropped. `applyMigration`
   (`Migration.purs`) does:

   ```purescript
   CreateTable t
     | tableExists t.name schema -> Left ("CreateTable: table `" <> t.name <> "` already exists")
     | otherwise                 -> Right (schema { tables = Array.snoc schema.tables t })
   ```

   So the model has no way to represent "this CREATE silently no-ops and
   leaves the *old, divergent* table." Without that, the analyzer never sees
   that `project_id` is absent.

4. **The RI check is FK-only.** `Migration.Safety.checkSchemaRI` (line 68)
   walks `t.foreignKeys` and emits `MissingTargetColumn` only for FK
   `refColumns`. An index referencing an absent column isn't checked.

The good news: the *shape* is already here. `MissingTargetColumn` is exactly
the right verdict; `checkOneFK`'s column-presence logic (line 72) is exactly
the right primitive; the trace-based "introduced / resolved / standing"
framing (`checkStep`) and the Alloy temporal cross-check (3b) are the
substrate. This is an extension, not a rewrite.

## Plan

Four ordered steps plus an ingestion path. Steps 1–2 are mechanical; step 3
is the real modelling work; step 4 is small once 1–3 land.

### Step 1 — parse `CREATE INDEX`

`MinardDB.Migration.SQL`: accept

```
CREATE [UNIQUE] INDEX [IF NOT EXISTS] name ON [schema.]table ( col [, col]* );
DROP INDEX [IF EXISTS] name;
```

Keep it the same deliberately-small subset spirit as the rest of the parser.
Emit the new constructors from step 2. Strip-comment handling already exists.

**Acceptance:** `parseSql` round-trips a script containing a `CREATE INDEX`
into the expected `MigrationSequence`; an index on a multi-column tuple
parses; `IF NOT EXISTS` / `UNIQUE` are preserved (not discarded — see step 3
for why the flag must survive).

### Step 2 — model indexes in the `Migration` ADT

`MinardDB.Migration`: add

```purescript
| CreateIndex { name :: String, table :: String, columns :: Array String
              , unique :: Boolean, ifNotExists :: Boolean }
| DropIndex   { name :: String, ifExists :: Boolean }
```

The `Schema`/`Table` AST gains an `indexes` field (or a parallel registry).
`applyMigration` adds/removes index records. `describeMigration` and
`describeIssue` get their cases. (Index-as-schema-object also sets up a
future "redundant index" lint, but that's out of scope here.)

**Acceptance:** `applyMigration` threads indexes through the trace; an index
on a present column applies cleanly; ADT changes compile with the existing
smoke fixtures untouched.

### Step 3 — effective-schema semantics for `IF NOT EXISTS` (the core)

This is the genuinely new modelling, and the reason the bug is subtle.

Add an `ifNotExists :: Boolean` flag to `CreateTable` (and honor the one we
keep on `AddColumn`/`CreateIndex`). Change `applyMigration` so the
conditional creators model **DDL no-op semantics** instead of erroring:

```purescript
CreateTable t
  | tableExists t.name schema && t.ifNotExists -> Right schema          -- no-op: keep existing (possibly divergent) table
  | tableExists t.name schema                  -> Left "...already exists"
  | otherwise                                  -> Right (add t)
```

The load-bearing consequence: when analysis starts from a schema whose
`annotations` table **predates `project_id`**, the no-op'd `CreateTable`
leaves that divergent table in place — so the column genuinely is absent for
the rest of the trace, until the `ALTER ... ADD COLUMN` runs. That is what
makes the later index reference a `MissingTargetColumn`.

This requires the analyzer to run from a **non-empty starting schema** — the
"effective" schema as it exists on a target database — not only from empty.
The catalog-introspection path (Phase 1, `tools/introspect-duckdb.py`)
already produces exactly such a starting `Schema`. Feeding a *fresh
loader-created* DB's introspected schema as the start state is the realistic
scenario and the one that reproduces the bug.

**Acceptance:** starting from a schema with a `project_id`-less
`annotations`, applying the real migration sequence (no-op CREATE → CREATE
INDEX on `project_id` → ALTER add column) leaves `project_id` absent at the
index step; starting from empty, the same sequence has it present (the CREATE
isn't a no-op) — the two starting states give different verdicts, which is
the whole point.

### Step 4 — extend RI check to index columns

`MinardDB.Migration.Safety`: add an index analogue of `checkOneFK`. For each
index, every `columns` entry must exist on its table; otherwise emit
`MissingTargetColumn` (reuse the constructor, or add a sibling
`MissingIndexColumn` if the FK-specific `fk`/`refColumn` fields don't fit —
a small `Issue` refactor toward `{ table, object, column }` is cleaner).
Fold it into `checkSchemaRI` so the trace diff (`introduced` / `resolved` /
`standing`) covers indexes for free.

**Acceptance:** on the trace from step 3's buggy starting state, the
`CREATE INDEX idx_annotations_project` step reports an *introduced*
missing-column issue naming `annotations.project_id`; reorder so the `ALTER`
precedes the index and the issue disappears — matching the actual fix that
shipped in `minard`.

### Ingestion — lift embedded SQL out of the server source

The migrations don't live in `.sql` files; they're `DB.exec db """..."""`
string literals in `minard`'s `server/src/Main.purs`, and the loader's
`database/schema/*.sql`. To run on the *real* migrations:

- Short term: a small extractor that pulls the triple-quoted SQL blocks from
  a PureScript source file and concatenates them in order, then hands them to
  `parseSql`. Minard already parses these source files into its DB — the
  string-literal SQL is in the AST, so the extractor can read it from there
  rather than re-parsing PureScript.
- The `database/schema/*.sql` files feed `parseSql` directly.

This is also where lesson #2 connects: the *starting* effective schema comes
from the loader's schema SQL, the *migration* comes from the server's
embedded SQL, and the bug is precisely that those two definition sites of
`annotations` diverged. Phase 3d models that divergence; a future
cross-artifact check would flag the divergence itself.

## Fixture & cross-check

Mirror the existing `Migration.Smoke` discipline (and the 3b Alloy
cross-check):

- `annotations-safe` — start empty, full sequence with ALTER before INDEX.
  Expect deterministic walker: no issues; Alloy: PROVEN.
- `annotations-unsafe` — start from the *divergent pre-existing* schema
  (no `project_id`), real shipped order (no-op CREATE → INDEX → ALTER).
  Expect walker: `MissingTargetColumn(annotations.project_id)` introduced at
  the INDEX step; Alloy: BROKEN (SAT) with the index step as witness.

The deliverable, as with 3a/3b, is the *agreement* of the deterministic
walker and the bounded Alloy model on both fixtures. The new wrinkle Alloy
must encode is the `ifNotExists` no-op transition (a `CreateTable` whose
guard is satisfied becomes a stutter), so the temporal model's
declared-vs-effective gap matches the walker's.

## Scope / non-goals

- Still the deliberately-small DDL subset. No partial indexes, expression
  indexes, `INCLUDE`, opclasses, triggers, views.
- No "redundant / unused index" analysis (future, rides on step 2's model).
- The PureScript-source SQL extractor is a pragmatic bridge, not a general
  PureScript parser — Minard's existing ingestion is the real source.
- Cross-artifact divergence detection (lesson #2) is noted, not built here.

## Open questions

1. **`Issue` shape.** Keep `MissingTargetColumn` FK-specific and add
   `MissingIndexColumn`, or generalize to `{ table, object, column, kind }`?
   The latter is cleaner for the trace-diff equality but touches the Alloy
   name generation and the frontend's issue rendering.
2. **Starting-schema provenance in the trace.** The "effective schema" start
   state needs to be a first-class, recorded input (which DB / snapshot it
   was introspected from), so a verdict is reproducible and the
   declared-vs-effective distinction is auditable.
3. **`AddColumn IF NOT EXISTS` symmetry.** Same no-op modelling should apply
   to `ALTER ... ADD COLUMN IF NOT EXISTS` (and `DropColumn IF EXISTS`); fold
   into step 3 rather than treating CreateTable specially.
