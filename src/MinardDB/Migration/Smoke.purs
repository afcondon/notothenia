-- | Smoke tests for the migration model + safety pass.
-- |
-- | Three fixtures exercise the two ways a migration can break
-- | referential integrity:
-- |
-- |   safeSequence       — incremental schema build, no issues
-- |   unsafeDropTable    — DROP TABLE removes the target of an FK
-- |   unsafeDropColumn   — DROP COLUMN removes the column an FK targets
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Migration.Smoke
module MinardDB.Migration.Smoke where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import MinardDB.Migration (Migration(..), MigrationSequence, describe, runSequence)
import MinardDB.Migration.Safety (StepReport, describeIssue, runSafetyReport)
import MinardDB.Schema (Column, FKAction(..), ForeignKey, PGType(..), Schema, Table)

------------------------------------------------------------------------
-- Initial schema: empty (every sequence starts from scratch)
------------------------------------------------------------------------

empty :: Schema
empty = { name: "migration-smoke", tables: [] }

------------------------------------------------------------------------
-- Building blocks
------------------------------------------------------------------------

usersTable :: Table
usersTable =
  { name: "users"
  , schemaName: "main"
  , columns:
      [ idCol
      , { name: "name", dataType: PGText, nullable: false, defaultExpr: Nothing }
      ]
  , primaryKey: [ "id" ]
  , foreignKeys: []
  , uniqueConstraints: []
  , functionalDependencies: []
  }

postsTable :: Table
postsTable =
  { name: "posts"
  , schemaName: "main"
  , columns:
      [ idCol
      , { name: "title", dataType: PGText, nullable: false, defaultExpr: Nothing }
      ]
  , primaryKey: [ "id" ]
  , foreignKeys: []
  , uniqueConstraints: []
  , functionalDependencies: []
  }

idCol :: Column
idCol = { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }

authorIdCol :: Column
authorIdCol = { name: "author_id", dataType: PGInt, nullable: false, defaultExpr: Nothing }

authorFK :: ForeignKey
authorFK =
  { columns: [ "author_id" ]
  , refTable: "users"
  , refColumns: [ "id" ]
  , onDelete: NoAction
  , onUpdate: NoAction
  }

------------------------------------------------------------------------
-- Sequences
------------------------------------------------------------------------

-- | The incremental build: users → posts → author_id → FK. No issues.
safeSequence :: MigrationSequence
safeSequence =
  [ CreateTable usersTable
  , CreateTable postsTable
  , AddColumn "posts" authorIdCol
  , AddForeignKey "posts" authorFK
  ]

-- | safe + DROP TABLE users.  The FK posts(author_id) -> users(id) is
-- | now dangling at the table level.
unsafeDropTable :: MigrationSequence
unsafeDropTable = safeSequence <> [ DropTable "users" ]

-- | safe + DROP COLUMN users.id.  The table still exists, but the
-- | column the FK targets is gone.
unsafeDropColumn :: MigrationSequence
unsafeDropColumn = safeSequence <> [ DropColumn "users" "id" ]

------------------------------------------------------------------------
-- Reporting
------------------------------------------------------------------------

main :: Effect Unit
main = do
  runOne "SAFE     " safeSequence
  Console.log ""
  runOne "UNSAFE-T " unsafeDropTable
  Console.log ""
  runOne "UNSAFE-C " unsafeDropColumn

runOne :: String -> MigrationSequence -> Effect Unit
runOne label ms = case runSequence empty ms of
  Left err -> do
    Console.log $ "[" <> label <> "] PARTIAL  — " <> err.error
    Console.log $ "  " <> show (Array.length err.partialTrace) <> " steps applied before failure"
  Right trace -> do
    let report = runSafetyReport trace
    Console.log $ "[" <> label <> "] " <> show (Array.length report.steps)
      <> " steps, " <> show report.totalIntroduced <> " issue(s) introduced"
    traverse_ printStep (Array.mapWithIndex (\ix step -> { ix, step }) report.steps)
    case report.finalStanding of
      [] -> Console.log "  ✓  no standing issues at end of sequence"
      issues -> do
        Console.log $ "  ⚠  " <> show (Array.length issues)
          <> " standing issue(s) at end of sequence:"
        traverse_ (\i -> Console.log ("       " <> describeIssue i)) issues

printStep :: { ix :: Int, step :: StepReport } -> Effect Unit
printStep r = do
  let prefix = "  step " <> show (r.ix + 1) <> ": "
  Console.log $ prefix <> describe r.step.migration
  case r.step.introduced of
    [] -> pure unit
    issues -> traverse_ (\i -> Console.log ("       ⚠  " <> describeIssue i)) issues
  case r.step.resolved of
    [] -> pure unit
    issues -> traverse_ (\i -> Console.log ("       ✓  resolved: " <> describeIssue i)) issues
