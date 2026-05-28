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
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Migration (Migration(..), MigrationSequence, describe, runSequence)
import MinardDB.Migration.Alloy (generateTemporal)
import MinardDB.Migration.Safety (StepReport, describeIssue, runSafetyReport)
import MinardDB.Schema (Column, FKAction(..), ForeignKey, PGType(..), Schema, Table)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

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
main = launchAff_ do
  runOne "safe"     safeSequence
  Console.log ""
  runOne "unsafe-t" unsafeDropTable
  Console.log ""
  runOne "unsafe-c" unsafeDropColumn

-- | Run both passes on a sequence: the deterministic static safety
-- | check, then the bounded Alloy 6 temporal check. The two should
-- | agree on whether RI is preserved — if they don't, one of them
-- | has a bug, and that disagreement is itself useful signal.
runOne :: String -> MigrationSequence -> Aff Unit
runOne label ms = case runSequence empty ms of
  Left err -> liftEffect do
    Console.log $ "[" <> label <> "] PARTIAL  — " <> err.error
    Console.log $ "  " <> show (Array.length err.partialTrace) <> " steps applied before failure"
  Right trace -> do
    let report = runSafetyReport trace
    liftEffect do
      Console.log $ "[" <> label <> "] " <> show (Array.length report.steps)
        <> " steps applied"
      Console.log "  static pass:"
      Console.log $ "    " <> show report.totalIntroduced <> " issue(s) introduced over the sequence"
      traverse_ printStep (Array.mapWithIndex (\ix step -> { ix, step }) report.steps)
      case report.finalStanding of
        [] -> Console.log "    ✓  no standing issues at end of sequence"
        issues -> do
          Console.log $ "    ⚠  " <> show (Array.length issues)
            <> " standing issue(s) at end of sequence:"
          traverse_ (\i -> Console.log ("       " <> describeIssue i)) issues
    runTemporalPass label ms

-- | Generate the Alloy 6 temporal model, hand it to Alloy, parse
-- | the receipt, and report whether `RIPreserved` holds.
runTemporalPass :: String -> MigrationSequence -> Aff Unit
runTemporalPass label ms = case generateTemporal label empty ms of
  Left err -> liftEffect $ Console.log $ "  temporal pass: gen error — " <> err.error
  Right modelText -> do
    let alsPath = "/tmp/migration-" <> label <> ".als"
    FS.writeTextFile UTF8 alsPath modelText
    liftEffect $ Console.log $ "  temporal pass (Alloy 6):"
    liftEffect $ Console.log $ "    wrote " <> alsPath
    result <- runAlloy defaultConfig alsPath
    let receiptPath = Path.concat [ "migration-" <> label, "receipt.json" ]
    receiptText <- FS.readTextFile UTF8 receiptPath
    case parseReceipt receiptText of
      Left err -> liftEffect $ Console.log $ "    receipt parse error: " <> err
      Right cmds -> liftEffect do
        Console.log $ "    Alloy exit " <> show result.exitCode
        traverse_ printTemporalVerdict cmds

printTemporalVerdict :: CommandResult -> Effect Unit
printTemporalVerdict r = Console.log $ "    " <> r.name <> ": " <> show r.verdict
  <> " — " <> case r.kind, r.verdict of
    Check, NoCounterexample -> "PROVEN (RI holds across all reachable traces within scope)"
    Check, Counterexample -> "BROKEN (RI fails at some step — see Alloy trace markdown)"
    Run, _ -> show r.verdict

printStep :: { ix :: Int, step :: StepReport } -> Effect Unit
printStep r = do
  let prefix = "    step " <> show (r.ix + 1) <> ": "
  Console.log $ prefix <> describe r.step.migration
  case r.step.introduced of
    [] -> pure unit
    issues -> traverse_ (\i -> Console.log ("         ⚠  " <> describeIssue i)) issues
  case r.step.resolved of
    [] -> pure unit
    issues -> traverse_ (\i -> Console.log ("         ✓  resolved: " <> describeIssue i)) issues
