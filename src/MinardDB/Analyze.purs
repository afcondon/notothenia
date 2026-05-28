module MinardDB.Analyze where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Alloy.Generate (generate)
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Schema (Schema)
import MinardDB.Schema.JSON (ParsedSchema, parseSchemaFull)
import MinardDB.Storage (AnalysisRecord, defaultStorageConfig, store)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path
import Node.Process as Process

-- | Read a Schema-JSON file (from tools/introspect-duckdb.py), generate
-- | an Alloy model, run Alloy, report verdicts, and persist.
analyze :: String -> Aff Unit
analyze jsonPath = do
  jsonText <- FS.readTextFile UTF8 jsonPath
  case parseSchemaFull jsonText of
    Left err -> Console.log $ "schema parse error: " <> err
    Right parsed -> analyzeParsed jsonPath parsed

analyzeParsed :: String -> ParsedSchema -> Aff Unit
analyzeParsed sourcePath parsed = do
  let declaredFKs = totalFKs parsed.declared
  let inferredCount = Array.length parsed.inferredFKs
  Console.log $ "Loaded schema: " <> parsed.declared.name <> " ("
    <> show (Array.length parsed.declared.tables) <> " tables, "
    <> show declaredFKs <> " declared FKs, "
    <> show inferredCount <> " inferred FKs)"
  when (declaredFKs == 0 && inferredCount > 0) do
    Console.log "  ⚠  NO DECLARED FOREIGN KEYS — referential integrity is application-mediated"
    Console.log "  Inferred FKs (from <entity>_id naming convention):"
    traverse_ (Console.log <<< ("    " <> _)) (formatInferredFKs parsed.withInferred)
  let scheme = if declaredFKs > 0 then parsed.declared else parsed.withInferred
  let label = if declaredFKs > 0
        then "declared FKs"
        else "inferred FKs (proof shows hypothetical)"
  Console.log $ "Running proof against " <> label <> "…"
  let alsPath = "/tmp/" <> scheme.name <> ".als"
  FS.writeTextFile UTF8 alsPath (generate scheme)
  result <- runAlloy defaultConfig alsPath
  let receiptPath = Path.concat [ scheme.name, "receipt.json" ]
  receiptText <- FS.readTextFile UTF8 receiptPath
  case parseReceipt receiptText of
    Left err -> Console.log $ "receipt parse error: " <> err
    Right cmds -> do
      Console.log $ show (Array.length cmds) <> " commands, exit "
        <> show result.exitCode <> ":"
      Console.log $ "  " <> formatHeader
      traverse_ (Console.log <<< ("  " <> _) <<< formatRow) cmds
      witnesses <- traverse (fetchWitness scheme.name) cmds
      traverse_ printWitness (Array.zip cmds witnesses)
      persist sourcePath parsed declaredFKs inferredCount cmds witnesses

persist
  :: String
  -> ParsedSchema
  -> Int
  -> Int
  -> Array CommandResult
  -> Array (Maybe String)
  -> Aff Unit
persist sourcePath parsed declaredFKs inferredCount cmds witnesses = do
  let
    proofs = Array.zipWith
      (\c w -> { command: c, witness: w, scope: 8 })
      cmds
      witnesses
    record :: AnalysisRecord
    record =
      { name: parsed.declared.name
      , sourcePath
      , tableCount: Array.length parsed.declared.tables
      , declaredFKCount: declaredFKs
      , inferredFKCount: inferredCount
      , inferredFKs: parsed.inferredFKs
      , proofs
      }
  outcome <- store defaultStorageConfig record
  case outcome of
    Right id -> Console.log $ "→ stored as analysis_id " <> show id
    Left err -> Console.log $ "⚠  storage failed: " <> err

-- | Read the solution markdown and pull out the witness atom, if any.
fetchWitness :: String -> CommandResult -> Aff (Maybe String)
fetchWitness schemaName cmd = case cmd.kind, cmd.verdict of
  Check, Counterexample -> do
    let solPath = Path.concat [ schemaName, cmd.name <> "-solution-0.md" ]
    text <- FS.readTextFile UTF8 solPath
    pure $ Array.head (extractWitnesses text)
  _, _ -> pure Nothing

printWitness :: Tuple CommandResult (Maybe String) -> Aff Unit
printWitness (Tuple cmd mw) = case mw of
  Just w -> do
    Console.log ""
    Console.log $ "  ⚠  " <> cmd.name <> " counterexample witness:"
    Console.log $ "     " <> w
  Nothing -> pure unit

extractWitnesses :: String -> Array String
extractWitnesses text =
  text
    # String.split (String.Pattern "\n")
    # Array.filter (String.contains (String.Pattern "{"))
    # Array.mapMaybe parseWitnessLine

parseWitnessLine :: String -> Maybe String
parseWitnessLine line =
  let
    afterBrace = String.split (String.Pattern "{") line
  in
    case Array.index afterBrace 1 of
      Nothing -> Nothing
      Just rest ->
        let beforeBrace = String.split (String.Pattern "}") rest
        in case Array.index beforeBrace 0 of
          Just atom | String.contains (String.Pattern "$") atom -> Just atom
          _ -> Nothing

totalFKs :: Schema -> Int
totalFKs schema = Array.length (Array.concatMap _.foreignKeys schema.tables)

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
