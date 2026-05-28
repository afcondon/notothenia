module MinardDB.Analyze where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Node.Process as Process
import MinardDB.Alloy.Generate (generate)
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Schema (Schema)
import MinardDB.Schema.JSON (parseSchemaFull)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

-- | Read a Schema-JSON file (produced by tools/introspect-duckdb.py),
-- | generate an Alloy model, run Alloy, and report verdicts.
analyze :: String -> Aff Unit
analyze jsonPath = do
  jsonText <- FS.readTextFile UTF8 jsonPath
  case parseSchemaFull jsonText of
    Left err -> Console.log $ "schema parse error: " <> err
    Right parsed -> do
      let declaredFKs = totalFKs parsed.declared
      Console.log $ "Loaded schema: " <> parsed.declared.name <> " ("
        <> show (Array.length parsed.declared.tables) <> " tables, "
        <> show declaredFKs <> " declared FKs, "
        <> show parsed.inferredFKCount <> " inferred FKs)"
      when (declaredFKs == 0 && parsed.inferredFKCount > 0) do
        Console.log "  ⚠  NO DECLARED FOREIGN KEYS — referential integrity is application-mediated"
        Console.log "  Inferred FKs (from <entity>_id naming convention):"
        traverse_ (Console.log <<< ("    " <> _)) (formatInferredFKs parsed.withInferred)
      let scheme = if declaredFKs > 0 then parsed.declared else parsed.withInferred
      let label = if declaredFKs > 0 then "declared FKs" else "inferred FKs (proof shows hypothetical)"
      Console.log $ "Running proof against " <> label <> "…"
      let alsPath = "/tmp/" <> scheme.name <> ".als"
      FS.writeTextFile UTF8 alsPath (generate scheme)
      result <- runAlloy defaultConfig alsPath
      let receiptPath = Path.concat [ scheme.name, "receipt.json" ]
      receiptText <- FS.readTextFile UTF8 receiptPath
      case parseReceipt receiptText of
        Left err ->
          Console.log $ "receipt parse error: " <> err
        Right cmds -> do
          Console.log $ show (Array.length cmds) <> " commands, exit "
            <> show result.exitCode <> ":"
          Console.log $ "  " <> formatHeader
          traverse_ (Console.log <<< ("  " <> _) <<< formatRow) cmds
          traverse_ (explainCounterexample scheme.name) cmds

totalFKs :: Schema -> Int
totalFKs schema =
  schema.tables # Array.concatMap _.foreignKeys # Array.length

formatInferredFKs :: Schema -> Array String
formatInferredFKs schema =
  schema.tables # Array.concatMap \t ->
    t.foreignKeys # map \fk ->
      padR 24 t.name
        <> "(" <> padR 16 (String.joinWith "," fk.columns)
        <> ") -> " <> fk.refTable

formatHeader :: String
formatHeader =
  padR 28 "command" <> padR 8 "kind" <> padR 8 "verdict" <> "interpretation"

formatRow :: CommandResult -> String
formatRow r =
  padR 28 r.name
    <> padR 8 (show r.kind)
    <> padR 8 (show r.verdict)
    <> interpretation r

interpretation :: CommandResult -> String
interpretation r = case r.kind, r.verdict of
  Check, NoCounterexample -> "PROVEN (no counterexample within scope)"
  Check, Counterexample -> "BROKEN (counterexample exists)"
  Run, Counterexample -> "instance found"
  Run, NoCounterexample -> "no satisfying instance"

-- | For each failed `check`, read the solution markdown and pull out
-- | the witness sig name(s).
explainCounterexample :: String -> CommandResult -> Aff Unit
explainCounterexample schemaName r = case r.kind, r.verdict of
  Check, Counterexample -> do
    let solPath = Path.concat [ schemaName, r.name <> "-solution-0.md" ]
    text <- FS.readTextFile UTF8 solPath
    let witnesses = extractWitnesses text
    Console.log ""
    Console.log $ "  ⚠  " <> r.name <> " counterexample witness:"
    case witnesses of
      [] -> Console.log "     (could not parse witness from solution markdown)"
      ws -> traverse_ (\w -> Console.log $ "     " <> w) ws
  _, _ -> pure unit

-- | Look for the `skolem` table in the Alloy solution markdown:
-- |    │$NoFKCycle_a│{module_namespaces$7}│
-- | and extract the bare atom names.
extractWitnesses :: String -> Array String
extractWitnesses text =
  text
    # String.split (String.Pattern "\n")
    # Array.filter (String.contains (String.Pattern "{"))
    # Array.mapMaybe parseWitnessLine

-- | Match lines like:  │$NoFKCycle_a│{module_namespaces$7}│
parseWitnessLine :: String -> Maybe String
parseWitnessLine line =
  let
    afterBrace = String.split (String.Pattern "{") line
  in
    case Array.index afterBrace 1 of
      Nothing -> Nothing
      Just rest ->
        let
          beforeBrace = String.split (String.Pattern "}") rest
        in
          case Array.index beforeBrace 0 of
            Just atom | String.contains (String.Pattern "$") atom -> Just atom
            _ -> Nothing

padR :: Int -> String -> String
padR n s =
  let
    len = String.length s
    pad = if len >= n then "" else String.joinWith "" (Array.replicate (n - len) " ")
  in
    s <> pad

main :: Effect Unit
main = launchAff_ do
  args <- liftEffect $ Array.drop 2 <$> Process.argv
  let jsonPath = case Array.head args of
        Just p -> p
        Nothing -> "/tmp/minard-schema.json"
  analyze jsonPath
