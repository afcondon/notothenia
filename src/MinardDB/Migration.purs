-- | Schema migrations as first-class values.
-- |
-- | A `Migration` is a single declarative DDL-equivalent step
-- | (CreateTable, AddColumn, AddForeignKey, etc.). A
-- | `MigrationSequence` is an ordered list, applied left-to-right to
-- | evolve a `Schema` through intermediate states.
-- |
-- | This module covers the data model + state-transition function.
-- | `MinardDB.Migration.Safety` consumes the resulting trace to check
-- | each step for referential-integrity issues; a future
-- | `MinardDB.Migration.Alloy` will emit an Alloy 6 temporal model
-- | over the same trace for full proof-backed verification.
-- |
-- | Why hand-coded migrations and not SQL parsing for now? The point of
-- | Phase 3a is to nail the *semantics* (what does a step do to a
-- | schema, what counts as breaking RI). SQL parsing is mechanical and
-- | lands in Phase 3c once the semantics are stable.
module MinardDB.Migration
  ( Migration(..)
  , MigrationSequence
  , TraceStep
  , applyMigration
  , runSequence
  , describe
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import MinardDB.Schema (Column, ForeignKey, Schema, Table)

-- | A single declarative migration step. Each constructor mirrors a
-- | family of DDL statements but stays at the schema-AST level so the
-- | semantics are explicit and we don't need a SQL parser to test the
-- | pipeline.
-- |
-- | Identification conventions:
-- |   - Tables are identified by `name :: String`
-- |   - Columns are identified by `(table :: String, column :: String)`
-- |   - Foreign keys are identified by `(table :: String,
-- |     sourceColumns :: Array String)` -- the column list pins the FK
-- |     down within a table that may have several FKs
data Migration
  = CreateTable Table
  | DropTable String
  | AddColumn String Column
  | DropColumn String String
  | AddForeignKey String ForeignKey
  | DropForeignKey String (Array String)

type MigrationSequence = Array Migration

-- | One step of a sequence: the migration, the schema *before* it, and
-- | the schema *after* it. Carrying both states makes the safety pass
-- | able to ask "what was previously valid that this step invalidates?"
-- | without re-deriving the deltas.
type TraceStep =
  { migration :: Migration
  , before :: Schema
  , after :: Schema
  }

-- | A short human-readable label for use in reports.
describe :: Migration -> String
describe = case _ of
  CreateTable t -> "CREATE TABLE " <> t.name
  DropTable n -> "DROP TABLE " <> n
  AddColumn t c -> "ALTER TABLE " <> t <> " ADD COLUMN " <> c.name
  DropColumn t c -> "ALTER TABLE " <> t <> " DROP COLUMN " <> c
  AddForeignKey t fk ->
    "ALTER TABLE " <> t <> " ADD FOREIGN KEY (" <> joinWith ", " fk.columns
      <> ") REFERENCES " <> fk.refTable <> "(" <> joinWith ", " fk.refColumns <> ")"
  DropForeignKey t cols ->
    "ALTER TABLE " <> t <> " DROP FOREIGN KEY (" <> joinWith ", " cols <> ")"

------------------------------------------------------------------------
-- applyMigration: pure schema-to-schema transition
------------------------------------------------------------------------

-- | Apply one migration to a schema, returning the next schema or an
-- | error if the migration is meaningless in the current state
-- | (dropping a non-existent table, adding a duplicate column, etc.).
-- |
-- | These pre-condition errors are different from RI safety errors -
-- | this layer only catches "this DDL would be syntactically rejected
-- | by the database". RI issues like "this DROP removes an FK target"
-- | flow through cleanly here so the safety pass can flag them with
-- | context.
applyMigration :: Migration -> Schema -> Either String Schema
applyMigration m schema = case m of
  CreateTable t
    | tableExists t.name schema ->
        Left ("CreateTable: table `" <> t.name <> "` already exists")
    | otherwise ->
        Right (schema { tables = Array.snoc schema.tables t })

  DropTable n
    | not (tableExists n schema) ->
        Left ("DropTable: table `" <> n <> "` does not exist")
    | otherwise ->
        Right (schema { tables = Array.filter (\t -> t.name /= n) schema.tables })

  AddColumn tName col -> updateTable tName schema \t ->
    if columnExists col.name t then
      Left ("AddColumn: column `" <> col.name <> "` already exists on `" <> tName <> "`")
    else
      Right (t { columns = Array.snoc t.columns col })

  DropColumn tName cName -> updateTable tName schema \t ->
    if not (columnExists cName t) then
      Left ("DropColumn: column `" <> cName <> "` does not exist on `" <> tName <> "`")
    else
      Right
        ( t
            { columns = Array.filter (\c -> c.name /= cName) t.columns
            -- If the dropped column was part of an FK source, drop the
            -- FK definition too. Real DDL would refuse without CASCADE,
            -- but for the static analyzer it's cleaner to roll the FK
            -- drop into the column drop; the safety pass will flag the
            -- *target side* if anything depended on it.
            , foreignKeys = Array.filter (\fk -> not (Array.elem cName fk.columns)) t.foreignKeys
            -- Same for unique constraints.
            , uniqueConstraints =
                Array.filter (\uc -> not (Array.elem cName uc.columns)) t.uniqueConstraints
            -- And any FD that mentions the column.
            , functionalDependencies =
                Array.filter
                  ( \fd -> not (Array.elem cName fd.determinant)
                      && not (Array.elem cName fd.dependent)
                  )
                  t.functionalDependencies
            }
        )

  AddForeignKey tName fk -> updateTable tName schema \t ->
    if fkExists fk.columns t then
      Left ("AddForeignKey: FK on `" <> tName <> "(" <> joinWith ", " fk.columns
        <> ")` already exists")
    else
      Right (t { foreignKeys = Array.snoc t.foreignKeys fk })

  DropForeignKey tName cols -> updateTable tName schema \t ->
    if not (fkExists cols t) then
      Left ("DropForeignKey: no FK on `" <> tName <> "(" <> joinWith ", " cols <> ")`")
    else
      Right (t { foreignKeys = Array.filter (\fk -> fk.columns /= cols) t.foreignKeys })

-- | Apply a whole sequence, accumulating a trace. Stops at the first
-- | applyMigration error (i.e. a syntactically invalid step) and
-- | reports both the partial trace and the error so the caller can
-- | render what was successfully applied.
runSequence :: Schema -> MigrationSequence -> Either { partialTrace :: Array TraceStep, error :: String } (Array TraceStep)
runSequence initial migrations =
  let
    go acc current ms = case Array.uncons ms of
      Nothing -> Right acc
      Just { head: m, tail } -> case applyMigration m current of
        Left err -> Left
          { partialTrace: acc
          , error: "step " <> show (Array.length acc + 1) <> " — "
              <> describe m <> ": " <> err
          }
        Right next ->
          let step = { migration: m, before: current, after: next }
          in go (Array.snoc acc step) next tail
  in
    go [] initial migrations

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

tableExists :: String -> Schema -> Boolean
tableExists n s = Array.any (\t -> t.name == n) s.tables

columnExists :: String -> Table -> Boolean
columnExists n t = Array.any (\c -> c.name == n) t.columns

fkExists :: Array String -> Table -> Boolean
fkExists cols t = Array.any (\fk -> fk.columns == cols) t.foreignKeys

updateTable
  :: String
  -> Schema
  -> (Table -> Either String Table)
  -> Either String Schema
updateTable tName schema f = case Array.findIndex (\t -> t.name == tName) schema.tables of
  Nothing -> Left ("table `" <> tName <> "` does not exist")
  Just ix -> case Array.index schema.tables ix of
    Nothing -> Left ("internal: index out of bounds")
    Just t -> case f t of
      Left err -> Left err
      Right t' -> case Array.updateAt ix t' schema.tables of
        Nothing -> Left ("internal: updateAt failed")
        Just tables' -> Right (schema { tables = tables' })

