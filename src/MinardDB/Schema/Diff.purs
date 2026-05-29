-- | Structural diff between two `Schema`s — the drift detector.
-- |
-- | Use case (extension-ladder rung 1, reverse half): parse an app's
-- | hand-written yoga `Table` declarations into a `Schema`
-- | (`MinardDB.Codegen.YogaParse`), introspect/parse the live database
-- | into another `Schema`, and diff. A non-empty diff means the typed
-- | bindings have drifted from the real database — a real bug.
-- |
-- | The diff is *structural*: tables/columns added or removed, column
-- | type or nullability changes, primary-key changes. It deliberately
-- | does NOT compare default-expression text — that field is lossy
-- | through the codegen round-trip (see `YogaTable`), so comparing it
-- | would manufacture false drift. Type comparison uses `pgTypeEquiv`,
-- | which treats the representation-collapse classes as equal
-- | (`Text`≈`Varchar`, `Int`≈`BigInt`) so the known-lossy mappings
-- | don't show up as drift either — only genuine type changes do.
module MinardDB.Schema.Diff
  ( Diff(..)
  , diffSchemas
  , pgTypeEquiv
  , describeDiff
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import MinardDB.Schema (Column, PGType(..), Schema, Table)

data Diff
  = TableOnlyInLeft String
  | TableOnlyInRight String
  | ColumnOnlyInLeft String String        -- table, column
  | ColumnOnlyInRight String String
  | ColumnTypeChanged String String PGType PGType   -- table, column, left, right
  | ColumnNullChanged String String Boolean Boolean -- table, column, leftNullable, rightNullable
  | PrimaryKeyChanged String (Array String) (Array String)

derive instance eqDiff :: Eq Diff

-- | True when two PG types are "the same" for drift purposes — i.e.
-- | equal, or in the same representation-collapse class that the codegen
-- | can't distinguish (so they never round-trip as drift).
pgTypeEquiv :: PGType -> PGType -> Boolean
pgTypeEquiv a b = canon a == canon b
  where
  canon = case _ of
    PGVarchar _ -> "text"
    PGText -> "text"
    PGBigInt -> "int"
    PGInt -> "int"
    other -> show other

-- | Diff `left` (the reference, e.g. the catalog) against `right` (e.g.
-- | the yoga types). Returned diffs read left-relative.
diffSchemas :: Schema -> Schema -> Array Diff
diffSchemas left right =
  tableDiffs <> Array.concatMap commonTableDiffs commonNames
  where
  leftNames = map _.name left.tables
  rightNames = map _.name right.tables

  tableDiffs =
    map TableOnlyInLeft (Array.difference leftNames rightNames)
      <> map TableOnlyInRight (Array.difference rightNames leftNames)

  commonNames = Array.intersect leftNames rightNames

  commonTableDiffs name =
    case findTable name left, findTable name right of
      Just lt, Just rt -> diffTable lt rt
      _, _ -> []

diffTable :: Table -> Table -> Array Diff
diffTable lt rt =
  colPresence <> colChanges <> pkDiff
  where
  leftCols = map _.name lt.columns
  rightCols = map _.name rt.columns

  colPresence =
    map (ColumnOnlyInLeft lt.name) (Array.difference leftCols rightCols)
      <> map (ColumnOnlyInRight lt.name) (Array.difference rightCols leftCols)

  colChanges = Array.intersect leftCols rightCols # Array.concatMap \cn ->
    case findColumn cn lt, findColumn cn rt of
      Just lc, Just rc ->
        (if pgTypeEquiv lc.dataType rc.dataType then []
         else [ ColumnTypeChanged lt.name cn lc.dataType rc.dataType ])
          <>
            (if lc.nullable == rc.nullable then []
             else [ ColumnNullChanged lt.name cn lc.nullable rc.nullable ])
      _, _ -> []

  pkDiff =
    if sortA lt.primaryKey == sortA rt.primaryKey then []
    else [ PrimaryKeyChanged lt.name (sortA lt.primaryKey) (sortA rt.primaryKey) ]

------------------------------------------------------------------------
-- rendering
------------------------------------------------------------------------

describeDiff :: Diff -> String
describeDiff = case _ of
  TableOnlyInLeft t -> "table `" <> t <> "` only on the left (missing from the right)"
  TableOnlyInRight t -> "table `" <> t <> "` only on the right (missing from the left)"
  ColumnOnlyInLeft t c -> "`" <> t <> "." <> c <> "` only on the left"
  ColumnOnlyInRight t c -> "`" <> t <> "." <> c <> "` only on the right"
  ColumnTypeChanged t c l r ->
    "`" <> t <> "." <> c <> "` type differs: " <> show l <> " (left) vs " <> show r <> " (right)"
  ColumnNullChanged t c l r ->
    "`" <> t <> "." <> c <> "` nullability differs: " <> nb l <> " (left) vs " <> nb r <> " (right)"
  PrimaryKeyChanged t l r ->
    "`" <> t <> "` primary key differs: [" <> joinWith ", " l <> "] vs [" <> joinWith ", " r <> "]"
  where
  nb true = "nullable"
  nb false = "not null"

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

findTable :: String -> Schema -> Maybe Table
findTable n s = Array.find (\t -> t.name == n) s.tables

findColumn :: String -> Table -> Maybe Column
findColumn n t = Array.find (\c -> c.name == n) t.columns

sortA :: Array String -> Array String
sortA = Array.sort
