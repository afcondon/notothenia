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
  , renderVarSigs
  , renderInit initial cat
  , renderTransitions
  , renderTrace cat trace
  , renderRIInvariant
  ]

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

renderVarSigs :: String
renderVarSigs =
  "// ── TIME-VARYING MEMBERSHIP ─────────────────────────────────────\n"
    <> "var sig ActiveTable  in Table  {}\n"
    <> "var sig ActiveColumn in Column {}\n"
    <> "var sig ActiveFK     in FK     {}"

renderInit :: Schema -> Catalog -> String
renderInit initial cat =
  "// ── INITIAL STATE ───────────────────────────────────────────────\n"
    <> "fact init {\n"
    <> renderInitBody initial cat
    <> "\n}"
  where
    renderInitBody s _ =
      if Array.null s.tables then
        "  no ActiveTable\n  no ActiveColumn\n  no ActiveFK"
      else
        "  // initial schema non-empty — populate from supplied state\n"
          <> "  ActiveTable  = " <> initTableSet cat s <> "\n"
          <> "  ActiveColumn = " <> initColumnSet cat s <> "\n"
          <> "  ActiveFK     = " <> initFKSet cat s

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
    , "pred createTable[t: Table, cols: set Column] {"
    , "  t not in ActiveTable"
    , "  ActiveTable'  = ActiveTable + t"
    , "  ActiveColumn' = ActiveColumn + cols"
    , "  ActiveFK'     = ActiveFK"
    , "}"
    , ""
    , "pred dropTable[t: Table] {"
    , "  t in ActiveTable"
    , "  ActiveTable'  = ActiveTable - t"
    , "  ActiveColumn' = ActiveColumn - ofTable.t"
    , "  ActiveFK'     = ActiveFK"
    , "}"
    , ""
    , "pred addColumn[c: Column] {"
    , "  c not in ActiveColumn"
    , "  c.ofTable in ActiveTable"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn + c"
    , "  ActiveFK'     = ActiveFK"
    , "}"
    , ""
    , "pred dropColumn[c: Column] {"
    , "  c in ActiveColumn"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn - c"
    , "  ActiveFK'     = ActiveFK"
    , "}"
    , ""
    , "pred addFK[f: FK] {"
    , "  f not in ActiveFK"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK + f"
    , "}"
    , ""
    , "pred dropFK[f: FK] {"
    , "  f in ActiveFK"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK - f"
    , "}"
    , ""
    , "pred stutter {"
    , "  ActiveTable'  = ActiveTable"
    , "  ActiveColumn' = ActiveColumn"
    , "  ActiveFK'     = ActiveFK"
    , "}"
    ]

-- | The trace fact: each migration step appears as `after^n predicate`,
-- | then a tail-stutter pins the system stable past the final step.
renderTrace :: Catalog -> Array TraceStep -> String
renderTrace cat trace =
  let
    rendered = Array.mapWithIndex (renderStep cat) trace
    nSteps = Array.length trace
    tail = afterChain nSteps <> "always stutter"
  in
    "// ── TRACE FACT ──────────────────────────────────────────────────\n"
      <> "fact trace {\n"
      <> joinWith "\n" (map indent rendered)
      <> "\n" <> indent tail
      <> "\n}"
  where
    indent s = "  " <> s

    renderStep cat' ix step = afterChain ix <> renderMigration cat' step.migration

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
    in
      "createTable[" <> tSig <> ", " <> colExpr <> "]"
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

renderRIInvariant :: String
renderRIInvariant =
  joinWith "\n"
    [ "// ── RI INVARIANT ────────────────────────────────────────────────"
    , "pred RIHolds {"
    , "  all f: ActiveFK |"
    , "    f.tgtTable in ActiveTable"
    , "    and f.tgtCols in ActiveColumn"
    , "}"
    , ""
    , "assert RIPreserved { always RIHolds }"
    , "check RIPreserved for 5 but 1..15 steps"
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

