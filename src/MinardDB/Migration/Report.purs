-- | Generate migration-verification reports as JSON, for the frontend
-- | timeline view (Phase 3e).
-- |
-- | This follows the same precompute → store → serve → render pattern
-- | as `MinardDB.Analyze`: the slow work (running the static safety
-- | pass and spawning Alloy) happens here, offline, and the result is
-- | a compact JSON file per migration sequence. The backend serves
-- | those files; the frontend renders them. We do NOT run Alloy on a
-- | web request — it spawns a JVM and takes seconds.
-- |
-- | A `MigrationReport` carries everything the timeline needs:
-- |   * the ordered steps, each with its description and the count of
-- |     standing RI issues *after* that step (the running count is what
-- |     colours each timeline node);
-- |   * the two Alloy verdicts (schema-level / row-level);
-- |   * the headline numbers (total introduced, final standing).
-- |
-- | The interesting reports are the ones where a step's standing count
-- | spikes above zero and then returns to zero — RI broken *in transit*
-- | while the endpoints stay clean. The registry-dev fixture is exactly
-- | that shape.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Migration.Report
module MinardDB.Migration.Report
  ( MigrationReport
  , StepCell
  , buildReport
  , encodeReport
  , main
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (Aff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandResult, Verdict(..), parseReceipt)
import MinardDB.Migration (MigrationSequence, describe, runSequence)
import MinardDB.Migration.Alloy (generateTemporal)
import MinardDB.Migration.SQL (dbmateUp, parseSql)
import MinardDB.Migration.Safety (StepReport, describeIssue, runSafetyReport)
import MinardDB.Migration.Smoke (empty, safeSequence, unsafeDropColumn, unsafeDropTable)
import MinardDB.Schema (Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

------------------------------------------------------------------------
-- Report model
------------------------------------------------------------------------

type StepCell =
  { index :: Int
  , description :: String        -- "DROP TABLE jobs"
  , introduced :: Array String   -- describeIssue strings
  , resolved :: Array String
  , standingAfter :: Int         -- # of dangling FKs after this step
  }

type MigrationReport =
  { name :: String
  , blurb :: String              -- one-line description for the list view
  , schemaVerdict :: String      -- PROVEN | BROKEN | UNKNOWN
  , rowVerdict :: String
  , totalIntroduced :: Int
  , finalStandingCount :: Int
  , steps :: Array StepCell
  }

------------------------------------------------------------------------
-- Building a report
------------------------------------------------------------------------

-- | Build the static portion of a report from the safety pass. The two
-- | Alloy verdicts are filled in separately (they require spawning the
-- | solver, which is an Aff).
buildReport
  :: String
  -> String
  -> { schemaVerdict :: String, rowVerdict :: String }
  -> Array StepReport
  -> MigrationReport
buildReport name blurb verdicts steps =
  let
    report = runSafetyReportFromSteps steps
  in
    { name
    , blurb
    , schemaVerdict: verdicts.schemaVerdict
    , rowVerdict: verdicts.rowVerdict
    , totalIntroduced: report.totalIntroduced
    , finalStandingCount: Array.length report.finalStanding
    , steps: Array.mapWithIndex toCell steps
    }
  where
  toCell ix s =
    { index: ix
    , description: describe s.migration
    , introduced: map describeIssue s.introduced
    , resolved: map describeIssue s.resolved
    , standingAfter: Array.length s.standing
    }
  -- `runSafetyReport` takes a TraceStep array, but a StepReport already
  -- carries before/after; re-derive the aggregate numbers directly.
  runSafetyReportFromSteps ss =
    { totalIntroduced: Array.length (Array.concatMap _.introduced ss)
    , finalStanding: case Array.last ss of
        Just last -> last.standing
        Nothing -> []
    }

------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------

encodeReport :: MigrationReport -> Json
encodeReport r = J.fromObject $ Object.fromFoldable
  [ "name"               /\ J.fromString r.name
  , "blurb"              /\ J.fromString r.blurb
  , "schemaVerdict"      /\ J.fromString r.schemaVerdict
  , "rowVerdict"         /\ J.fromString r.rowVerdict
  , "totalIntroduced"    /\ J.fromNumber (Int.toNumber r.totalIntroduced)
  , "finalStandingCount" /\ J.fromNumber (Int.toNumber r.finalStandingCount)
  , "steps"              /\ J.fromArray (map encodeStep r.steps)
  ]

encodeStep :: StepCell -> Json
encodeStep s = J.fromObject $ Object.fromFoldable
  [ "index"         /\ J.fromNumber (Int.toNumber s.index)
  , "description"   /\ J.fromString s.description
  , "introduced"    /\ J.fromArray (map J.fromString s.introduced)
  , "resolved"      /\ J.fromArray (map J.fromString s.resolved)
  , "standingAfter" /\ J.fromNumber (Int.toNumber s.standingAfter)
  ]

------------------------------------------------------------------------
-- Generation pipeline
------------------------------------------------------------------------

-- | Output directory for the JSON reports. Kept clear of the
-- | `migration-*/` namespace (which holds Alloy's per-run output dirs
-- | and is gitignored) so these committed demo fixtures aren't swept up.
reportsDir :: String
reportsDir = "reports"

registryDir :: String
registryDir =
  "/Users/afc/work/afc-work/GitHub/local-copies/registry-dev/db/migrations"

registryFiles :: Array String
registryFiles =
  [ "20230711143615_create_jobs_table.sql"
  , "20230711143803_create_logs_table.sql"
  , "20240914170550_delete_jobs_logs_table.sql"
  , "20240914171030_create_job_queue_tables.sql"
  ]

main :: Effect Unit
main = launchAff_ do
  -- Make sure the output directory exists (idempotent).
  _ <- attempt (FS.mkdir reportsDir)

  liftEffect $ Console.log "Generating migration reports…"

  generate "safe"
    "Incremental build, no issues — both invariants proven."
    empty safeSequence
  generate "unsafe-t"
    "DROP TABLE removes an FK's target table — schema and rows both break."
    empty unsafeDropTable
  generate "unsafe-c"
    "DROP COLUMN removes an FK's target column — schema breaks, rows fine."
    empty unsafeDropColumn

  -- The real-world one: read the dbmate files, slice up-sections, parse.
  bodies <- traverse readRegistryUp registryFiles
  case parseSql (Array.intercalate "\n" bodies) of
    Left err -> liftEffect $ Console.log $ "  registry-dev PARSE ERROR — " <> err
    Right ms ->
      generate "registry-dev"
        "PureScript Registry dbmate history — RI broken in transit, endpoints clean."
        empty ms

  liftEffect $ Console.log "Done."

readRegistryUp :: String -> Aff String
readRegistryUp f = do
  txt <- FS.readTextFile UTF8 (Path.concat [ registryDir, f ])
  pure (dbmateUp txt)

-- | Run a sequence through both passes, build the report, write JSON.
generate :: String -> String -> Schema -> MigrationSequence -> Aff Unit
generate name blurb initial ms = case runSequence initial ms of
  Left err -> liftEffect $ Console.log $ "  [" <> name <> "] apply failed: " <> err.error
  Right trace -> do
    let report0 = runSafetyReport trace
    verdicts <- alloyVerdicts name initial ms
    let report = buildReport name blurb verdicts report0.steps
    let outPath = Path.concat [ reportsDir, name <> ".json" ]
    FS.writeTextFile UTF8 outPath (stringify (encodeReport report))
    liftEffect $ Console.log $ "  [" <> name <> "] schema=" <> verdicts.schemaVerdict
      <> " row=" <> verdicts.rowVerdict
      <> " → " <> outPath

-- | Generate the Alloy temporal model, run it, parse the receipt into
-- | the two named verdicts. On any failure the verdicts degrade to
-- | "UNKNOWN" rather than aborting the whole generation run.
alloyVerdicts
  :: String
  -> Schema
  -> MigrationSequence
  -> Aff { schemaVerdict :: String, rowVerdict :: String }
alloyVerdicts name initial ms = case generateTemporal name initial ms of
  Left _ -> pure unknown
  Right modelText -> do
    -- Alloy writes its receipt to a directory named after the .als
    -- file's stem, in the cwd. So `/tmp/migration-report-<name>.als`
    -- produces `./migration-report-<name>/receipt.json`.
    let stem = "migration-report-" <> name
    let alsPath = "/tmp/" <> stem <> ".als"
    FS.writeTextFile UTF8 alsPath modelText
    _ <- runAlloy defaultConfig alsPath
    let receiptPath = Path.concat [ stem, "receipt.json" ]
    attempt (FS.readTextFile UTF8 receiptPath) >>= case _ of
      Left _ -> pure unknown
      Right receiptText -> case parseReceipt receiptText of
        Left _ -> pure unknown
        Right cmds -> pure
          { schemaVerdict: verdictOf "SchemaRIPreserved" cmds
          , rowVerdict: verdictOf "RowRIPreserved" cmds
          }
  where
  unknown = { schemaVerdict: "UNKNOWN", rowVerdict: "UNKNOWN" }

verdictOf :: String -> Array CommandResult -> String
verdictOf cmdName cmds = case Array.find (\c -> c.name == cmdName) cmds of
  Nothing -> "UNKNOWN"
  Just c -> case c.verdict of
    NoCounterexample -> "PROVEN"
    Counterexample -> "BROKEN"
