-- | Field test: point the SQL parser at a real, ordered migration
-- | history from the wild and run the full verification pipeline.
-- |
-- | The subject is the PureScript Registry's dbmate migration set
-- | (`registry-dev/db/migrations/*.sql`), applied in timestamp order.
-- | Its history happens to contain exactly the failure mode notothenia
-- | exists to catch:
-- |
-- |   1. create `jobs`
-- |   2. create `logs` with an FK → jobs ON DELETE CASCADE
-- |   3. DROP TABLE jobs; DROP TABLE logs;   ← the destructive step
-- |   4. recreate everything under a new `job_info`-centred schema
-- |
-- | Within step 3 the `jobs` table is dropped while `logs` still
-- | carries its FK to it — a transient dangling foreign key. The
-- | migration *ends* in a consistent state (both tables gone, then a
-- | fresh schema built), so a naive "is the final schema OK?" check
-- | passes. The temporal `always RIHolds` assertion does not: it sees
-- | the intermediate state where RI is violated. That contrast — final
-- | state clean, trace dirty — is the whole point of doing this in
-- | temporal logic rather than diffing endpoints.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Migration.SQL.FieldTest
module MinardDB.Migration.SQL.FieldTest where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.String (joinWith)
import Data.Traversable (traverse)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Migration (MigrationSequence, describe, runSequence)
import MinardDB.Migration.Alloy (generateTemporal)
import MinardDB.Migration.SQL (dbmateUp, parseSql)
import MinardDB.Migration.Safety (StepReport, describeIssue, runSafetyReport)
import MinardDB.Schema (Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

-- | The migration directory, in dependency/timestamp order. dbmate
-- | sorts lexicographically by the timestamp prefix; we list the files
-- | explicitly so the field test is self-describing and doesn't depend
-- | on a directory read returning sorted names.
migrationDir :: String
migrationDir =
  "/Users/afc/work/afc-work/GitHub/local-copies/registry-dev/db/migrations"

migrationFiles :: Array String
migrationFiles =
  [ "20230711143615_create_jobs_table.sql"
  , "20230711143803_create_logs_table.sql"
  , "20240914170550_delete_jobs_logs_table.sql"
  , "20240914171030_create_job_queue_tables.sql"
  ]

empty :: Schema
empty = { name: "registry-dev", tables: [] }

main :: Effect Unit
main = launchAff_ do
  liftEffect $ Console.log "── Field test: registry-dev dbmate migration history ──"
  liftEffect $ Console.log ""

  -- Read every file, slice out its `up` section, concatenate in order.
  bodies <- traverse readUp migrationFiles
  let combined = joinWith "\n" bodies

  case parseSql combined of
    Left err -> liftEffect do
      Console.log $ "PARSE ERROR — " <> err
      Console.log "  (real-world DDL hit a construct the parser doesn't accept;"
      Console.log "   the message above carries the position)"
    Right ms -> do
      liftEffect do
        Console.log $ "Parsed " <> show (Array.length ms) <> " migration(s):"
        traverse_ (\m -> Console.log ("    " <> describe m)) ms
        Console.log ""
      runPipeline ms

readUp :: String -> Aff String
readUp f = do
  txt <- FS.readTextFile UTF8 (Path.concat [ migrationDir, f ])
  pure (dbmateUp txt)

runPipeline :: MigrationSequence -> Aff Unit
runPipeline ms = case runSequence empty ms of
  Left err -> liftEffect do
    Console.log $ "APPLY FAILED at " <> err.error
    Console.log $ "  " <> show (Array.length err.partialTrace) <> " step(s) applied before failure"
  Right trace -> do
    let report = runSafetyReport trace
    liftEffect do
      Console.log "Static safety pass (step-by-step walk):"
      Console.log $ "  " <> show report.totalIntroduced
        <> " issue(s) introduced across the history"
      traverse_ printStep (Array.mapWithIndex (\ix step -> { ix, step }) report.steps)
      case report.finalStanding of
        [] -> do
          Console.log "  ✓  ZERO standing issues at the end of the history"
          Console.log "     → an endpoint-only check would call this migration clean."
        issues -> do
          Console.log $ "  ⚠  " <> show (Array.length issues) <> " standing issue(s) at end:"
          traverse_ (\i -> Console.log ("       " <> describeIssue i)) issues
      Console.log ""
    runTemporal ms

runTemporal :: MigrationSequence -> Aff Unit
runTemporal ms = case generateTemporal "registry-dev" empty ms of
  Left err -> liftEffect $ Console.log $ "Temporal pass: gen error — " <> err.error
  Right modelText -> do
    let alsPath = "/tmp/migration-registry-dev.als"
    FS.writeTextFile UTF8 alsPath modelText
    liftEffect do
      Console.log "Temporal pass (Alloy 6, `always RIHolds` across the whole trace):"
      Console.log $ "    wrote " <> alsPath
    result <- runAlloy defaultConfig alsPath
    let receiptPath = Path.concat [ "migration-registry-dev", "receipt.json" ]
    receiptText <- FS.readTextFile UTF8 receiptPath
    case parseReceipt receiptText of
      Left err -> liftEffect $ Console.log $ "    receipt parse error: " <> err
      Right cmds -> liftEffect do
        Console.log $ "    Alloy exit " <> show result.exitCode
        traverse_ printVerdict cmds
        Console.log ""
        Console.log "Reading: a BROKEN schema verdict here with ZERO standing"
        Console.log "issues above is the headline — RI is violated *in transit*"
        Console.log "(jobs dropped while logs still references it), even though"
        Console.log "the migration's endpoints are individually consistent."

printVerdict :: CommandResult -> Effect Unit
printVerdict r = Console.log $ "    " <> r.name <> ": " <> show r.verdict
  <> " — " <> case r.kind, r.verdict of
    Check, NoCounterexample -> "PROVEN (RI holds at every state in the trace)"
    Check, Counterexample -> "BROKEN (RI violated at some state — see Alloy trace)"
    Run, _ -> show r.verdict

printStep :: { ix :: Int, step :: StepReport } -> Effect Unit
printStep r = do
  Console.log $ "    step " <> show (r.ix + 1) <> ": " <> describe r.step.migration
  traverse_ (\i -> Console.log ("         ⚠  introduced: " <> describeIssue i)) r.step.introduced
  traverse_ (\i -> Console.log ("         ✓  resolved:   " <> describeIssue i)) r.step.resolved
