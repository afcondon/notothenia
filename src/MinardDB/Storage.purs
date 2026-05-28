module MinardDB.Storage where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (intercalate)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.FS.Aff as FS
import Node.Stream as Stream
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..))

-- | Where notothenia stores its analysis history.
type StorageConfig =
  { dbPath :: String
  , duckdbBinary :: String
  }

defaultStorageConfig :: StorageConfig
defaultStorageConfig =
  { dbPath: "/Users/afc/work/afc-work/CodeExplorer/minard/database/notothenia.duckdb"
  , duckdbBinary: "duckdb"
  }

-- | Everything we capture about a single analysis run.
type AnalysisRecord =
  { name :: String
  , sourcePath :: String
  , tableCount :: Int
  , declaredFKCount :: Int
  , inferredFKCount :: Int
  -- | One entry per inferred FK: (source_table, source_columns, ref_table, ref_columns)
  , inferredFKs :: Array { sourceTable :: String, columns :: Array String, refTable :: String, refColumns :: Array String }
  -- | One entry per Alloy command run.
  , proofs :: Array
      { command :: CommandResult
      , witness :: Maybe String
      , scope :: Int
      , minScope :: Maybe Int  -- populated by Minimize when a SAT check is shrunk
      }
  }

-- | Persist a record. Returns the analysis_id assigned by the DB.
store :: StorageConfig -> AnalysisRecord -> Aff (Either String Int)
store cfg rec = do
  let sql = generateInserts rec
  let tmp = "/tmp/notothenia-insert-" <> sanitizeName rec.name <> ".sql"
  FS.writeTextFile UTF8 tmp sql
  result <- runDuckDB cfg tmp
  if result.exitCode == 0 then
    case extractAnalysisId result.stdout of
      Just id -> pure $ Right id
      Nothing -> pure $ Left $ "could not parse analysis_id from: " <> result.stdout
  else
    pure $ Left $ "duckdb failed (exit " <> show result.exitCode <> "): "
      <> result.stderr <> " --- stdout: " <> result.stdout

-- | The generated INSERT script. Uses DuckDB's currval('seq_analyses')
-- | to attach child rows to the parent analysis row.
generateInserts :: AnalysisRecord -> String
generateInserts rec =
  intercalate "\n"
    [ "BEGIN;"
    , parentInsert
    , inferredFKsInsert
    , proofsInsert
    , "COMMIT;"
    , ".mode csv"
    , ".headers off"
    , "SELECT currval('seq_analyses');"
    ]
  where
    parentInsert =
      "INSERT INTO analyses (name, source_path, table_count, declared_fk_count, inferred_fk_count)\n"
        <> "  VALUES (" <> str rec.name
        <> ", " <> str rec.sourcePath
        <> ", " <> show rec.tableCount
        <> ", " <> show rec.declaredFKCount
        <> ", " <> show rec.inferredFKCount <> ");"

    inferredFKsInsert =
      if Array.null rec.inferredFKs then "-- no inferred FKs"
      else
        let
          values = rec.inferredFKs # map \fk ->
            "(currval('seq_analyses')"
              <> ", " <> str fk.sourceTable
              <> ", " <> str (String.joinWith "," fk.columns)
              <> ", " <> str fk.refTable
              <> ", " <> str (String.joinWith "," fk.refColumns) <> ")"
        in
          "INSERT INTO analysis_inferred_fks (analysis_id, source_table, source_columns, ref_table, ref_columns) VALUES\n  "
            <> intercalate ",\n  " values <> ";"

    proofsInsert =
      if Array.null rec.proofs then "-- no proofs"
      else
        let
          values = rec.proofs # map \p ->
            let c = p.command
                witness = case p.witness of
                  Just w -> str w
                  Nothing -> "NULL"
                minScope = case p.minScope of
                  Just n -> show n
                  Nothing -> "NULL"
            in
              "(currval('seq_analyses')"
                <> ", " <> str c.name
                <> ", " <> str (show c.kind)
                <> ", " <> str c.source
                <> ", " <> str (show c.verdict)
                <> ", " <> str (interpret c)
                <> ", " <> witness
                <> ", " <> show p.scope
                <> ", " <> minScope <> ")"
        in
          "INSERT INTO analysis_proofs (analysis_id, command_name, kind, source, verdict, interpretation, witness, scope, min_scope) VALUES\n  "
            <> intercalate ",\n  " values <> ";"

    interpret c = case c.kind, c.verdict of
      Check, NoCounterexample -> "PROVEN (no counterexample within scope)"
      Check, Counterexample -> "BROKEN (counterexample exists)"
      Run, Counterexample -> "instance found"
      Run, NoCounterexample -> "no satisfying instance"

-- SQL escaping: wrap in single quotes, doubling any embedded ones.
str :: String -> String
str s = "'" <> String.replaceAll (String.Pattern "'") (String.Replacement "''") s <> "'"

sanitizeName :: String -> String
sanitizeName = String.replaceAll (String.Pattern "/") (String.Replacement "_")

-- | Find the analysis_id in duckdb's output. The closing SELECT prints
-- | a small table; we look for the first standalone positive int.
extractAnalysisId :: String -> Maybe Int
extractAnalysisId stdout =
  stdout
    # String.split (String.Pattern "\n")
    # map String.trim
    # Array.mapMaybe Int.fromString
    # Array.filter (_ > 0)
    # Array.head

-- | Run duckdb against the given DB, executing the SQL file via `.read`.
runDuckDB :: StorageConfig -> String -> Aff { exitCode :: Int, stdout :: String, stderr :: String }
runDuckDB cfg sqlPath = makeAff \callback -> do
  cp <- CP.spawn cfg.duckdbBinary [ cfg.dbPath, ".read " <> sqlPath ]
  stdoutRef <- Ref.new ""
  stderrRef <- Ref.new ""
  CP.stdout cp # on_ Stream.dataH \chunk -> do
    s <- Buffer.toString UTF8 chunk
    Ref.modify_ (_ <> s) stdoutRef
  CP.stderr cp # on_ Stream.dataH \chunk -> do
    s <- Buffer.toString UTF8 chunk
    Ref.modify_ (_ <> s) stderrRef
  cp # on_ CP.exitH \exit -> do
    out <- Ref.read stdoutRef
    err <- Ref.read stderrRef
    let code = case exit of
          Normally n -> n
          BySignal _ -> -1
    callback (Right { exitCode: code, stdout: out, stderr: err })
  pure nonCanceler
