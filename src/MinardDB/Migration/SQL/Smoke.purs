-- | Smoke test for the SQL parser: hand-written `.sql` fixtures that
-- | mirror `MinardDB.Migration.Smoke`'s three sequences, fed through
-- | `parseSql` and then the same temporal pipeline.
-- |
-- | The fixtures here express the same intent as the hand-coded
-- | sequences in `Migration.Smoke` but as actual DDL text. If the
-- | parser is correct, the Alloy verdicts for these should be
-- | identical to the verdicts for the matching hand-coded fixtures.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Migration.SQL.Smoke
module MinardDB.Migration.SQL.Smoke where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Migration (MigrationSequence, describe, runSequence)
import MinardDB.Migration.Alloy (generateTemporal)
import MinardDB.Migration.SQL (parseSql)
import MinardDB.Migration.Safety (describeIssue, runSafetyReport)
import MinardDB.Schema (Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

------------------------------------------------------------------------
-- Fixtures
------------------------------------------------------------------------

-- | The same incremental build as Migration.Smoke.safeSequence,
-- | expressed as DDL.
safeSql :: String
safeSql =
  """
  CREATE TABLE users (
    id INT NOT NULL PRIMARY KEY,
    name TEXT NOT NULL
  );

  CREATE TABLE posts (
    id INT NOT NULL PRIMARY KEY,
    title TEXT NOT NULL
  );

  ALTER TABLE posts ADD COLUMN author_id INT NOT NULL;

  ALTER TABLE posts
    ADD FOREIGN KEY (author_id) REFERENCES users(id);
  """

-- | safe + DROP TABLE users — the FK target table goes away.
unsafeDropTableSql :: String
unsafeDropTableSql = safeSql <>
  """

  DROP TABLE users;
  """

-- | safe + DROP COLUMN users.id — the FK target column goes away.
unsafeDropColumnSql :: String
unsafeDropColumnSql = safeSql <>
  """

  ALTER TABLE users DROP COLUMN id;
  """

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

empty :: Schema
empty = { name: "sql-smoke", tables: [] }

main :: Effect Unit
main = launchAff_ do
  runOne "sql-safe" safeSql
  Console.log ""
  runOne "sql-unsafe-t" unsafeDropTableSql
  Console.log ""
  runOne "sql-unsafe-c" unsafeDropColumnSql

runOne :: String -> String -> Aff Unit
runOne label sql = case parseSql sql of
  Left err -> liftEffect do
    Console.log $ "[" <> label <> "] PARSE ERROR — " <> err
  Right ms -> do
    liftEffect $ Console.log $ "[" <> label <> "] parsed "
      <> show (Array.length ms) <> " migration(s)"
    traverse_ (\m -> liftEffect $ Console.log ("    " <> describe m)) ms
    case runSequence empty ms of
      Left err -> liftEffect do
        Console.log $ "  RUN PARTIAL — " <> err.error
      Right trace -> do
        let report = runSafetyReport trace
        liftEffect do
          Console.log "  static pass:"
          Console.log $ "    " <> show report.totalIntroduced
            <> " issue(s) introduced over the sequence"
          case report.finalStanding of
            [] -> Console.log "    ✓  no standing issues at end of sequence"
            issues -> do
              Console.log $ "    ⚠  " <> show (Array.length issues)
                <> " standing issue(s) at end of sequence:"
              traverse_ (\i -> Console.log ("       " <> describeIssue i)) issues
        runTemporalPass label ms

runTemporalPass :: String -> MigrationSequence -> Aff Unit
runTemporalPass label ms = case generateTemporal label empty ms of
  Left err -> liftEffect $ Console.log $ "  temporal pass: gen error — " <> err.error
  Right modelText -> do
    let alsPath = "/tmp/migration-" <> label <> ".als"
    FS.writeTextFile UTF8 alsPath modelText
    liftEffect $ Console.log "  temporal pass (Alloy 6):"
    liftEffect $ Console.log $ "    wrote " <> alsPath
    result <- runAlloy defaultConfig alsPath
    let receiptPath = Path.concat [ "migration-" <> label, "receipt.json" ]
    receiptText <- FS.readTextFile UTF8 receiptPath
    case parseReceipt receiptText of
      Left err -> liftEffect $ Console.log $ "    receipt parse error: " <> err
      Right cmds -> liftEffect do
        Console.log $ "    Alloy exit " <> show result.exitCode
        traverse_ printVerdict cmds

printVerdict :: CommandResult -> Effect Unit
printVerdict r = Console.log $ "    " <> r.name <> ": " <> show r.verdict
  <> " — " <> case r.kind, r.verdict of
    Check, NoCounterexample -> "PROVEN"
    Check, Counterexample -> "BROKEN"
    Run, _ -> show r.verdict
