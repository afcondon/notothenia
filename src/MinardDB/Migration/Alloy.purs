-- | Generate an Alloy 6 temporal model from a migration sequence.
-- |
-- | The encoding pattern:
-- |
-- |   * **Static catalog** — every table, column, and FK that appears
-- |     in any state of the trace becomes a one-sig declared up front.
-- |     Names are treated as stable identities: a table dropped and
-- |     re-created with the same name is the same sig with `Active`
-- |     membership going false then true again.
-- |
-- |   * **Time-varying membership** — `var sig ActiveTable in Table`
-- |     (likewise Column / FK). Initial state is empty (or whatever
-- |     the supplied `initial :: Schema` says).
-- |
-- |   * **Transition predicates** — one per migration kind, identical
-- |     across all generated models. Each constrains the next state.
-- |
-- |   * **Trace fact** — a single fact chaining the migration steps
-- |     with nested `after`s; tail-stutter pins the trace stable
-- |     beyond the final step.
-- |
-- |   * **Assertion** — `always RIHolds`. Alloy SAT means the trace
-- |     produces a state where some FK targets a dropped table or
-- |     dropped column.
-- |
-- | We deliberately keep migrations as the only source of change.
-- | Row-level data is out of scope for 3b; a future 3c will add `var`
-- | row sigs and turn this into a richer temporal-property checker.
module MinardDB.Migration.Alloy
  ( generateTemporal
  , staticCatalog
  , Catalog
  , TableEntry
  , ColumnEntry
  , FKEntry
  , RunErr
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Maybe (Maybe(..))
import Data.String (joinWith, replace, Pattern(..), Replacement(..))
import MinardDB.Migration (Migration(..), MigrationSequence, TraceStep, runSequence)
import MinardDB.Schema (Column, ForeignKey, Schema, Table)

-- | The union catalog: every entity that *ever* appears in any state
-- | of the trace. We need a stable identity per (tableName) and
-- | (tableName, columnName) and (tableName, fk-columns) so that an
-- | `Active*` set membership reflects the entity's current state.
type Catalog =
  { tables :: Array TableEntry
  , columns :: Array ColumnEntry
  , fks :: Array FKEntry
  }

type TableEntry =
  { name :: String
  , sigName :: String           -- e.g. "T_users"
  }

type ColumnEntry =
  { tableName :: String
  , name :: String
  , sigName :: String           -- e.g. "C_users_id"
  , ofTableSig :: String        -- e.g. "T_users"
  }

type FKEntry =
  { sourceTableName :: String
  , sourceColumns :: Array String
  , refTableName :: String
  , refColumns :: Array String
  , sigName :: String           -- e.g. "FK_posts_author_id"
  , srcTableSig :: String
  , srcColSigs :: Array String
  , tgtTableSig :: String
  , tgtColSigs :: Array String
  }

-- | Build the union catalog from a trace. Iterates every schema state
-- | (initial + after-each-step) so we pick up entities that exist only
-- | briefly (e.g. a column added then dropped).
staticCatalog :: Schema -> Array TraceStep -> Catalog
staticCatalog initial trace =
  let
    states :: Array Schema
    states = Array.cons initial (map _.after trace)
  in
    foldl mergeState emptyCatalog states
  where
    emptyCatalog = { tables: [], columns: [], fks: [] }

    mergeState :: Catalog -> Schema -> Catalog
    mergeState acc s = foldl mergeTable acc s.tables

    mergeTable :: Catalog -> Table -> Catalog
    mergeTable acc t =
      let
        acc1 = if hasTableEntry acc t.name
                 then acc
                 else acc { tables = Array.snoc acc.tables (mkTableEntry t.name) }
        acc2 = foldl (mergeColumn t.name) acc1 t.columns
        acc3 = foldl (mergeFK t.name) acc2 t.foreignKeys
      in
        acc3

    mergeColumn :: String -> Catalog -> Column -> Catalog
    mergeColumn tName acc c =
      if hasColumnEntry acc tName c.name
        then acc
        else acc { columns = Array.snoc acc.columns (mkColumnEntry tName c.name) }

    mergeFK :: String -> Catalog -> ForeignKey -> Catalog
    mergeFK srcTable acc fk =
      if hasFKEntry acc srcTable fk.columns
        then acc
        else acc { fks = Array.snoc acc.fks (mkFKEntry srcTable fk) }

    hasTableEntry c n = Array.any (\e -> e.name == n) c.tables
    hasColumnEntry c t n = Array.any (\e -> e.tableName == t && e.name == n) c.columns
    hasFKEntry c t cols = Array.any (\e -> e.sourceTableName == t && e.sourceColumns == cols) c.fks

    mkTableEntry n = { name: n, sigName: "T_" <> sanitize n }
    mkColumnEntry t n =
      { tableName: t
      , name: n
      , sigName: "C_" <> sanitize t <> "_" <> sanitize n
      , ofTableSig: "T_" <> sanitize t
      }
    mkFKEntry t fk =
      { sourceTableName: t
      , sourceColumns: fk.columns
      , refTableName: fk.refTable
      , refColumns: fk.refColumns
      , sigName: "FK_" <> sanitize t <> "_" <> joinWith "_" (map sanitize fk.columns)
      , srcTableSig: "T_" <> sanitize t
      , srcColSigs: map (\c -> "C_" <> sanitize t <> "_" <> sanitize c) fk.columns
      , tgtTableSig: "T_" <> sanitize fk.refTable
      , tgtColSigs: map (\c -> "C_" <> sanitize fk.refTable <> "_" <> sanitize c) fk.refColumns
      }

-- | Replace anything non-alphanumeric with underscore so the sig name
-- | is a valid Alloy identifier. Cheap; collisions are unlikely in
-- | practice but possible (e.g. `foo-bar` and `foo_bar` both -> `foo_bar`).
-- | Document the risk; tighten only if it shows up in real schemas.
sanitize :: String -> String
sanitize =
  replace (Pattern "-") (Replacement "_")
    <<< replace (Pattern ".") (Replacement "_")
    <<< replace (Pattern " ") (Replacement "_")

-- | Render an Alloy 6 temporal model. Returns Left if the migration
-- | sequence can't even be applied syntactically (e.g. dropping a
-- | table that doesn't exist) — in that case there's nothing to
-- | verify, the static layer already caught it.
generateTemporal :: String -> Schema -> MigrationSequence -> Either RunErr String
generateTemporal name initial ms = case runSequence initial ms of
  Left err -> Left err
  Right trace ->
    let
      cat = staticCatalog initial trace
    in
      Right (render name cat initial trace)

type RunErr = { partialTrace :: Array TraceStep, error :: String }

------------------------------------------------------------------------
-- Rendering
------------------------------------------------------------------------

render :: String -> Catalog -> Schema -> Array TraceStep -> String
render name cat initial trace = joinWith "\n\n"
  [ header name
  , renderTableSigs cat
  , renderColumnSigs cat
  , renderFKSigs cat
  , renderRowSig
  , renderVarSigs
  , renderInit initial cat
  , renderTransitions
  , renderTrace cat trace
  , renderRIInvariants (stepBound trace)
  ]

-- | Exact number of states the interleaved trace occupies: one
-- | migration state + one populate state per step, plus the final
-- | stutter state. The `check … but 1..N steps` bound MUST be at least
-- | this or Alloy silently under-checks the tail of a long migration
-- | history (the bound used to be a magic `20`, which quietly truncated
-- | any real-world sequence longer than ~9 migrations).
stepBound :: Array TraceStep -> Int
stepBound trace = 2 * Array.length trace + 1

header :: String -> String
header n =
  "// Generated by Notothenia (Migration.Alloy) for sequence: " <> n <> "\n"
    <> "// Alloy 6 temporal model. `var sig ActiveX` tracks which entities\n"
    <> "// exist at each step; the trace fact pins the transition sequence.\n"
    <> "// `assert RIPreserved` checks that no FK ever dangles."

renderTableSigs :: Catalog -> String
renderTableSigs cat =
  "// ── STATIC ENTITIES — TABLES ────────────────────────────────────\n"
    <> "abstract sig Table {}"
    <> joinWith "" (map (\e -> "\none sig " <> e.sigName <> " extends Table {}") cat.tables)

renderColumnSigs :: Catalog -> String
renderColumnSigs cat =
  "// ── STATIC ENTITIES — COLUMNS ───────────────────────────────────\n"
    <> "abstract sig Column { ofTable: one Table }"
    <> joinWith ""
        ( map
            ( \e -> "\none sig " <> e.sigName <> " extends Column {} { ofTable = "
                <> e.ofTableSig <> " }"
            )
            cat.columns
        )

renderFKSigs :: Catalog -> String
renderFKSigs cat =
  "// ── STATIC ENTITIES — FOREIGN KEYS ──────────────────────────────\n"
    <> "abstract sig FK {\n"
    <> "  srcTable: one Table,\n"
    <> "  srcCols:  set Column,\n"
    <> "  tgtTable: one Table,\n"
    <> "  tgtCols:  set Column\n"
    <> "}"
    <> joinWith "" (map renderOneFK cat.fks)
  where
    renderOneFK e =
      "\none sig " <> e.sigName <> " extends FK {} {\n"
        <> "  srcTable = " <> e.srcTableSig <> "\n"
        <> "  srcCols  = " <> joinCols e.srcColSigs <> "\n"
        <> "  tgtTable = " <> e.tgtTableSig <> "\n"
        <> "  tgtCols  = " <> joinCols e.tgtColSigs <> "\n"
        <> "}"

    joinCols [] = "none"
    joinCols xs = joinWith " + " xs

renderRowSig :: String
renderRowSig = joinWith "\n"
  [ "// ── ROWS ────────────────────────────────────────────────────────"
  , "// Rows belong to a table. A row may optionally reference another"
  , "// row via a specific FK; consistency facts pin the relations down."
  , "// Alloy chooses how many rows exist (within scope) and how they"
  , "// reference each other -- making row-level RI an explorable axis."
  , "sig Row {"
  , "  ofTable:  one Table,"
  , "  fkVia:    lone FK,"
  , "  fkTarget: lone Row"
  , "}"
  , ""
  , "fact rowFKConsistency {"
  , "  // fkVia and fkTarget either both present or both absent."
  , "  all r: Row | some r.fkVia iff some r.fkTarget"
  , "  // The fkVia FK's source table matches the row's table."
  , "  all r: Row | some r.fkVia implies r.fkVia.srcTable = r.ofTable"
  , "  // The fkTarget's table matches the fkVia FK's target table."
  , "  all r: Row | some r.fkVia implies r.fkTarget.ofTable = r.fkVia.tgtTable"
  , "}"
  ]

renderVarSigs :: String
renderVarSigs =
  "// ── TIME-VARYING MEMBERSHIP ─────────────────────────────────────\n"
    <> "var sig ActiveTable  in Table  {}\n"
    <> "var sig ActiveColumn in Column {}\n"
    <> "var sig ActiveFK     in FK     {}\n"
    <> "var sig ActiveRow    in Row    {}"

renderInit :: Schema -> Catalog -> String
renderInit initial cat =
  "// ── INITIAL STATE ───────────────────────────────────────────────\n"
    <> "fact init {\n"
    <> renderInitBody initial cat
    <> "\n}"
  where
    renderInitBody s _ =
      if Array.null s.tables then
        "  no ActiveTable\n  no ActiveColumn\n  no ActiveFK\n  no ActiveRow"
      else
        "  // initial schema non-empty — populate from supplied state\n"
          <> "  ActiveTable  = " <> initTableSet cat s <> "\n"
          <> "  ActiveColumn = " <> initColumnSet cat s <> "\n"
          <> "  ActiveFK     = " <> initFKSet cat s <> "\n"
          <> "  // Initial row population is unconstrained — Alloy chooses\n"
          <> "  // any valid set restricted to active-table rows.\n"
          <> "  ActiveRow in ofTable.ActiveTable"

    initTableSet cat' s =
      let
        sigs = Array.mapMaybe (\t -> tableSigFor cat' t.name) s.tables
      in
        if Array.null sigs then "none" else joinWith " + " sigs

    initColumnSet cat' s =
      let
        sigs = Array.concatMap
          (\t -> Array.mapMaybe (\c -> columnSigFor cat' t.name c.name) t.columns)
          s.tables
      in
        if Array.null sigs then "none" else joinWith " + " sigs

    initFKSet cat' s =
      let
        sigs = Array.concatMap
          (\t -> Array.mapMaybe (\fk -> fkSigFor cat' t.name fk.columns) t.foreignKeys)
          s.tables
      in
        if Array.null sigs then "none" else joinWith " + " sigs

renderTransitions :: String
renderTransitions =
  joinWith "\n"
    [ "// ── TRANSITION PREDICATES ───────────────────────────────────────"
    , "// Schema migrations leave rows unchanged EXCEPT for dropTable,"
    , "// which cascade-removes the dropped table's rows. Source rows in"
    , "// other tables that referenced them now dangle — the row-level"
    , "// RI violation we want surfaced."
    , "// createTable activates the table, its columns, AND any FKs"
    , "// declared inline in the CREATE TABLE (passed as `fks`). The"
    , "// inline-FK case is why this arg exists: a CREATE TABLE whose"
    , "// definition carries a FOREIGN KEY clause activates that FK at"
    , "// creation time, not via a later ALTER. Omitting it (the old"
    , "// `ActiveFK' = ActiveFK`) left inline FKs forever inactive, so"
    , "// the schema-RI invariant was vacuously true for them — a model"
    , "// bug the registry-dev field test surfaced."
    , "pred createTable[t: Table, cols: set Column, fks: set FK] {"
    , "  t not in ActiveTable"
    , "  ActiveTable'  = ActiveTable + t"
    , "  ActiveColumn' = ActiveColumn + cols"
    , "  ActiveFK'     = ActiveFK + fks"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    , ""
    , "// dropTable cascade-removes the dropped table's columns, its rows,"
    , "// and FKs *sourced from* it (a table's own FK constraints die with"
    , "// the table). FKs that *target* the dropped table are deliberately"
    , "// left active so they dangle — that orphaned-reference state is"
    , "// exactly the RI violation we want the trace to expose."
    , "pred dropTable[t: Table] {"
    , "  t in ActiveTable"
    , "  ActiveTable'  = ActiveTable - t"
    , "  ActiveColumn' = ActiveColumn - ofTable.t"
    , "  ActiveFK'     = ActiveFK - srcTable.t"
    , "  ActiveRow'    = ActiveRow - ofTable.t"
    , "}"
    , ""
    , "pred addColumn[c: Column] {"
    , "  c not in ActiveColumn"
    , "  c.ofTable in ActiveTable"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn + c"
    , "  ActiveFK'     = ActiveFK"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    , ""
    , "pred dropColumn[c: Column] {"
    , "  c in ActiveColumn"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn - c"
    , "  ActiveFK'     = ActiveFK"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    , ""
    , "pred addFK[f: FK] {"
    , "  f not in ActiveFK"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK + f"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    , ""
    , "pred dropFK[f: FK] {"
    , "  f in ActiveFK"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK - f"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    , ""
    , "// `populate` lets new rows enter ActiveRow. Alloy picks any"
    , "// row(s) whose tables are active AND whose fkTarget is also"
    , "// added when fkVia is set."
    , "//"
    , "// We require fkTarget presence whenever fkVia is set, NOT only"
    , "// when the FK is currently active. The weaker version ('fkVia"
    , "// in ActiveFK implies fkTarget in ActiveRow') admits row"
    , "// populations whose FK constraints aren't currently enforced;"
    , "// a subsequent addFK then *retroactively* breaks RI, surfacing"
    , "// as a spurious counterexample for the SAFE sequence too."
    , "// Treating fkVia as a row-shape commitment matches the real-DB"
    , "// intuition that a row referencing another row keeps doing so"
    , "// regardless of whether the FK is currently enforced."
    , "pred populate {"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK"
    , "  ActiveRow' in ofTable.ActiveTable"
    , "  all r: ActiveRow' | some r.fkVia implies r.fkTarget in ActiveRow'"
    , "}"
    , ""
    , "pred stutter {"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK"
    , "  ActiveRow'    = ActiveRow"
    , "}"
    ]

-- | The trace fact interleaves each migration with a `populate` step.
-- | For an N-step migration sequence, the trace has 2N+1 positions:
-- |
-- |   position 0       — migration[0]
-- |   position 1       — populate
-- |   position 2       — migration[1]
-- |   position 3       — populate
-- |   ...
-- |   position 2N-2    — migration[N-1]
-- |   position 2N-1    — populate
-- |   position 2N      — always stutter
-- |
-- | Why interleave? `populate` is what gives Alloy a freedom to introduce
-- | rows. With populate only at the very end, by the time a destructive
-- | migration has already fired the target rows can't exist and the row
-- | RI assertion is trivially satisfied. With populate before each
-- | destructive step, Alloy can place a row population that the next
-- | migration's cascade renders invalid — that's the counterexample
-- | the row-level assertion exists to surface.
renderTrace :: Catalog -> Array TraceStep -> String
renderTrace cat trace =
  let
    nSteps = Array.length trace
    migrationLines = Array.mapWithIndex (renderMigStep cat) trace
    populateLines = Array.mapWithIndex (\ix _ -> renderPopulate ix) trace
    interleaved = Array.concat (Array.zipWith (\m p -> [ m, p ]) migrationLines populateLines)
    tail = afterChain (2 * nSteps) <> "always stutter"
  in
    "// ── TRACE FACT ──────────────────────────────────────────────────\n"
      <> "fact trace {\n"
      <> joinWith "\n" (map indent interleaved)
      <> "\n" <> indent tail
      <> "\n}"
  where
    indent s = "  " <> s

    renderMigStep cat' ix step =
      afterChain (2 * ix) <> renderMigration cat' step.migration

    renderPopulate ix =
      afterChain (2 * ix + 1) <> "populate"

afterChain :: Int -> String
afterChain n
  | n <= 0 = ""
  | otherwise = joinWith "" (Array.replicate n "after ")

renderMigration :: Catalog -> Migration -> String
renderMigration cat = case _ of
  CreateTable t ->
    let
      tSig = lookupTableSig cat t.name
      colSigs = Array.mapMaybe (\c -> columnSigFor cat t.name c.name) t.columns
      colExpr = if Array.null colSigs then "none" else joinWith " + " colSigs
      fkSigs = Array.mapMaybe (\fk -> fkSigFor cat t.name fk.columns) t.foreignKeys
      fkExpr = if Array.null fkSigs then "none" else joinWith " + " fkSigs
    in
      "createTable[" <> tSig <> ", " <> colExpr <> ", " <> fkExpr <> "]"
  DropTable n ->
    "dropTable[" <> lookupTableSig cat n <> "]"
  AddColumn tName col ->
    "addColumn[" <> lookupColumnSig cat tName col.name <> "]"
  DropColumn tName cName ->
    "dropColumn[" <> lookupColumnSig cat tName cName <> "]"
  AddForeignKey tName fk ->
    "addFK[" <> lookupFKSig cat tName fk.columns <> "]"
  DropForeignKey tName cols ->
    "dropFK[" <> lookupFKSig cat tName cols <> "]"

renderRIInvariants :: Int -> String
renderRIInvariants bound = joinWith "\n"
  [ "// ── RI INVARIANTS ───────────────────────────────────────────────"
  , "// Split into schema-level and row-level so the verdicts diagnose"
  , "// which layer breaks. DROP TABLE on the target of an FK breaks"
  , "// both: the table is gone (so the FK schema-level dangles) AND"
  , "// the rows that referenced it are cascade-removed (so source"
  , "// rows in other tables are orphaned). DROP COLUMN of an FK"
  , "// target column breaks only schema-level — rows don't track"
  , "// which columns store their values. That's the fault-localization"
  , "// payoff: the two verdicts together describe whether the fix is"
  , "// a column-level rewire or a data-loss event."
  , "pred RISchemaLevel {"
  , "  all f: ActiveFK |"
  , "    f.tgtTable in ActiveTable"
  , "    and f.tgtCols in ActiveColumn"
  , "}"
  , ""
  , "// See `populate` for why `some r.fkVia` (not `r.fkVia in ActiveFK`)"
  , "// is the antecedent: a row whose FK target dangles is broken"
  , "// whether or not the FK constraint is currently enforced."
  , "pred RIRowLevel {"
  , "  all r: ActiveRow | r.ofTable in ActiveTable"
  , "  all r: ActiveRow | some r.fkVia implies r.fkTarget in ActiveRow"
  , "}"
  , ""
  , "assert SchemaRIPreserved { always RISchemaLevel }"
  , "assert RowRIPreserved    { always RIRowLevel }"
  , "check SchemaRIPreserved for 5 but 1.." <> show bound <> " steps"
  , "check RowRIPreserved    for 5 but 1.." <> show bound <> " steps"
  ]

------------------------------------------------------------------------
-- Lookup helpers
------------------------------------------------------------------------

tableSigFor :: Catalog -> String -> Maybe String
tableSigFor c n = (Array.find (\e -> e.name == n) c.tables) <#> _.sigName

columnSigFor :: Catalog -> String -> String -> Maybe String
columnSigFor c t n =
  Array.find (\e -> e.tableName == t && e.name == n) c.columns <#> _.sigName

fkSigFor :: Catalog -> String -> Array String -> Maybe String
fkSigFor c t cols =
  Array.find (\e -> e.sourceTableName == t && e.sourceColumns == cols) c.fks
    <#> _.sigName

-- The lookup* variants are partial — they assume the catalog covers
-- everything the migration touches, which is guaranteed by construction
-- (staticCatalog walked the same trace). A miss indicates a generator
-- bug, so we emit a syntactically-wrong placeholder that Alloy will
-- choke on, surfacing it loudly rather than silently emitting a model
-- that omits the step.
lookupTableSig :: Catalog -> String -> String
lookupTableSig c n = case tableSigFor c n of
  Just s -> s
  Nothing -> "MISSING_TABLE_" <> n

lookupColumnSig :: Catalog -> String -> String -> String
lookupColumnSig c t n = case columnSigFor c t n of
  Just s -> s
  Nothing -> "MISSING_COL_" <> t <> "_" <> n

lookupFKSig :: Catalog -> String -> Array String -> String
lookupFKSig c t cols = case fkSigFor c t cols of
  Just s -> s
  Nothing -> "MISSING_FK_" <> t <> "_" <> joinWith "_" cols

