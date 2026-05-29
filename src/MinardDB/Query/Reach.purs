-- | Resolve a query's raw references against a schema, and aggregate
-- | reach across a query set to find dead columns (Phase 4 — query
-- | reach analysis).
-- |
-- | `MinardDB.Query.parseQuery` gives the references a query *writes*
-- | (qualified, bare, wildcard). This module turns those into the
-- | concrete `(table, column)` pairs they actually *touch* in a given
-- | schema, applying SQL's name-resolution rules:
-- |
-- |   * a qualifier is an alias (resolve to its table) or a table name;
-- |   * a bare column resolves to whichever FROM table has a column of
-- |     that name — if exactly one does (the SQL unambiguity rule). Zero
-- |     candidates → unresolved (probably an expression alias or a name
-- |     we mis-harvested); more than one → ambiguous (counted against
-- |     every candidate, and reported);
-- |   * `*` expands to all columns of all FROM tables; `t.*` to all
-- |     columns of the table `t` resolves to.
-- |
-- | A `Reach` is the set of touched `(table, column)` pairs plus the
-- | diagnostics. Aggregating reach over every query in a codebase and
-- | subtracting from the schema's columns gives the **dead columns** —
-- | columns no query references. Because the parser errs toward
-- | over-collecting references, dead-column detection errs toward
-- | *under*-reporting: we never call a column dead that something might
-- | touch, only ones nothing demonstrably touches.
module MinardDB.Query.Reach
  ( ColumnId
  , Reach
  , DeadColumnReport
  , resolveReach
  , deadColumns
  , showColumnId
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import MinardDB.Query (QueryRefs, TableRef)
import MinardDB.Schema (Schema, Table)

-- | A fully-qualified column: which table, which column.
type ColumnId = { table :: String, column :: String }

showColumnId :: ColumnId -> String
showColumnId c = c.table <> "." <> c.column

-- | The result of resolving one query against a schema.
type Reach =
  { touched :: Array ColumnId       -- concrete (table,column) pairs reached
  , tables :: Array String          -- tables the query reads
  , ambiguous :: Array String       -- bare columns matching >1 FROM table
  , unresolved :: Array String      -- references that matched no schema column
  }

type DeadColumnReport =
  { dead :: Array ColumnId          -- schema columns no query referenced
  , reached :: Array ColumnId       -- the union of all reach
  , totalColumns :: Int
  }

------------------------------------------------------------------------
-- Single-query resolution
------------------------------------------------------------------------

resolveReach :: Schema -> QueryRefs -> Reach
resolveReach schema refs =
  let
    -- The tables actually in scope for this query (those that resolve to
    -- a real schema table). Unknown tables are dropped from scope but
    -- still reported via `tables`.
    fromTables :: Array { ref :: TableRef, table :: Table }
    fromTables = Array.mapMaybe
      (\tr -> map (\t -> { ref: tr, table: t }) (lookupTable tr.table schema))
      refs.tables

    resolved = Array.foldl (resolveOne fromTables) emptyAcc refs.columns
  in
    { touched: Array.nubByEq eqColId resolved.touched
    , tables: Array.nub (map _.table refs.tables)
    , ambiguous: Array.nub resolved.ambiguous
    , unresolved: Array.nub resolved.unresolved
    }
  where
  emptyAcc = { touched: [], ambiguous: [], unresolved: [] }

  resolveOne fromTables acc ref = case ref.qualifier, ref.column of
    -- bare `*` → every column of every in-scope FROM table
    Nothing, "*" ->
      acc { touched = acc.touched <> allColumns fromTables }

    -- `q.*` → every column of the table q resolves to
    Just q, "*" -> case resolveQualifier fromTables q of
      Just t -> acc { touched = acc.touched <> columnsOf t }
      Nothing -> acc { unresolved = Array.snoc acc.unresolved (q <> ".*") }

    -- `q.c` → (resolved table of q).c, if that column exists
    Just q, c -> case resolveQualifier fromTables q of
      Just t ->
        if hasColumn c t
          then acc { touched = Array.snoc acc.touched { table: t.name, column: c } }
          else acc { unresolved = Array.snoc acc.unresolved (q <> "." <> c) }
      Nothing -> acc { unresolved = Array.snoc acc.unresolved (q <> "." <> c) }

    -- bare `c` → the unique FROM table with a column c (SQL rule)
    Nothing, c ->
      let owners = Array.filter (\ft -> hasColumn c ft.table) fromTables
      in case owners of
        [ ft ] -> acc { touched = Array.snoc acc.touched { table: ft.table.name, column: c } }
        [] -> acc { unresolved = Array.snoc acc.unresolved c }
        many -> acc
          { touched = acc.touched <> map (\ft -> { table: ft.table.name, column: c }) many
          , ambiguous = Array.snoc acc.ambiguous c
          }

  -- A qualifier matches a table whose alias equals it, else whose name
  -- equals it.
  resolveQualifier fromTables q =
    case Array.find (\ft -> ft.ref.alias == Just q) fromTables of
      Just ft -> Just ft.table
      Nothing -> map _.table (Array.find (\ft -> ft.ref.table == q) fromTables)

  allColumns fromTables = Array.concatMap (\ft -> columnsOf ft.table) fromTables

  columnsOf t = map (\col -> { table: t.name, column: col.name }) t.columns

eqColId :: ColumnId -> ColumnId -> Boolean
eqColId a b = a.table == b.table && a.column == b.column

------------------------------------------------------------------------
-- Aggregate: dead columns across a query set
------------------------------------------------------------------------

-- | Every column in the schema that no query in the set references.
deadColumns :: Schema -> Array QueryRefs -> DeadColumnReport
deadColumns schema queries =
  let
    reached = Array.nubByEq eqColId
      (Array.concatMap (\q -> (resolveReach schema q).touched) queries)
    allCols = schema.tables # Array.concatMap \t ->
      map (\c -> { table: t.name, column: c.name }) t.columns
    dead = Array.filter (\c -> not (Array.any (eqColId c) reached)) allCols
  in
    { dead
    , reached
    , totalColumns: Array.length allCols
    }

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

lookupTable :: String -> Schema -> Maybe Table
lookupTable n s = Array.find (\t -> t.name == n) s.tables

hasColumn :: String -> Table -> Boolean
hasColumn n t = Array.any (\c -> c.name == n) t.columns
