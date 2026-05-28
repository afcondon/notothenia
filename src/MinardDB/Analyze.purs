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
import MinardDB.Alloy.Minimize (minimizeScope)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Properties (AlloyCheck, defaultProperties, defaultScope, interpretCommand)
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
      let catalog = defaultProperties >>= (_ $ scheme)
      Console.log $ show (Array.length cmds) <> " commands, exit "
        <> show result.exitCode <> ":"
      Console.log $ "  " <> formatHeader
      traverse_ (Console.log <<< ("  " <> _) <<< formatRow catalog) cmds
      witnesses <- traverse (fetchWitness scheme.name) cmds
      traverse_ printWitness (Array.zip cmds witnesses)
      let satCount = Array.length $ Array.filter
            (\c -> c.kind == Check && c.verdict == Counterexample) cmds
      when (satCount > 0) do
        Console.log ""
        Console.log $ "Minimizing scope for " <> show satCount
          <> " SAT verdict(s) (binary search downward)…"
      proofs <- traverse (buildProof catalog scheme) (Array.zip cmds witnesses)
      traverse_ printMinScope proofs
      persist sourcePath parsed declaredFKs inferredCount proofs

-- | A proof record carrying everything we need to persist a single
-- | Alloy command's outcome: the raw verdict, a human-readable
-- | interpretation derived from the AlloyCheck body kind, the witness
-- | atom (if any), the scope at which Alloy was invoked, and (for SAT
-- | verdicts) the smallest scope at which the property still breaks.
type ProofRecord =
  { command :: CommandResult
  , interpretation :: String
  , witness :: Maybe String
  , scope :: Int
  , minScope :: Maybe Int
  }

-- | Build a proof record from a command + witness. For SAT verdicts, runs
-- | scope minimization in the background (binary search downward) to find
-- | the smallest counterexample-producing scope.
buildProof :: Array AlloyCheck -> Schema -> Tuple CommandResult (Maybe String) -> Aff ProofRecord
buildProof catalog schema (Tuple cmd witness) = do
  let
    mcheck = Array.find (\c -> c.name == cmd.name) catalog
    scope = case mcheck of
      Just c -> c.scope
      Nothing -> defaultScope schema  -- e.g. for the synthetic `show` run
    interpretation = interpretCommand mcheck cmd
  minScope <- case cmd.kind, cmd.verdict, mcheck of
    Check, Counterexample, Just check -> minimizeScope defaultConfig schema check
    _, _, _ -> pure Nothing
  pure { command: cmd, interpretation, witness, scope, minScope }

printMinScope :: ProofRecord -> Aff Unit
printMinScope p = case p.minScope of
  Just m | m < p.scope ->
    Console.log $ "     ↓  " <> p.command.name
      <> " still SAT at scope " <> show m
      <> " (original scope " <> show p.scope <> ")"
  _ -> pure unit

persist
  :: String
  -> ParsedSchema
  -> Int
  -> Int
  -> Array ProofRecord
  -> Aff Unit
persist sourcePath parsed declaredFKs inferredCount proofs = do
  let
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

formatRow :: Array AlloyCheck -> CommandResult -> String
formatRow catalog r =
  padR 28 r.name
    <> padR 8 (show r.kind)
    <> padR 8 (show r.verdict)
    <> interpretCommand (Array.find (\c -> c.name == r.name) catalog) r

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
