module MinardDB.Read where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref
import Foreign.Object as Object
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.Stream as Stream

-- | Summary row from the analyses table.
type AnalysisSummary =
  { id :: Int
  , name :: String
  , sourcePath :: String
  , capturedAt :: String
  , tableCount :: Int
  , declaredFKCount :: Int
  , inferredFKCount :: Int
  }

-- | Detail rows for one analysis.
type AnalysisDetail =
  { summary :: AnalysisSummary
  , inferredFKs :: Array InferredFKRow
  , proofs :: Array ProofRow
  }

type InferredFKRow =
  { sourceTable :: String
  , sourceColumns :: String
  , refTable :: String
  , refColumns :: String
  }

type ProofRow =
  { commandName :: String
  , kind :: String
  , source :: String
  , verdict :: String
  , interpretation :: String
  , witness :: Maybe String
  , scope :: Int
  }

-- | Config for reading. We share dbPath / duckdbBinary with Storage.
type ReadConfig =
  { dbPath :: String
  , duckdbBinary :: String
  }

defaultReadConfig :: ReadConfig
defaultReadConfig =
  { dbPath: "/Users/afc/work/afc-work/CodeExplorer/minard/database/notothenia.duckdb"
  , duckdbBinary: "duckdb"
  }

-- | List all analyses (most recent first).
listAnalyses :: ReadConfig -> Aff (Either String (Array AnalysisSummary))
listAnalyses cfg = do
  let sql = "SELECT id, name, source_path, captured_at::VARCHAR AS captured_at, "
              <> "table_count, declared_fk_count, inferred_fk_count "
              <> "FROM analyses ORDER BY captured_at DESC"
  result <- runQuery cfg sql
  pure $ result >>= parseSummaryArray

-- | Get full detail for a single analysis. Returns Nothing if not found.
getAnalysis :: ReadConfig -> Int -> Aff (Either String (Maybe AnalysisDetail))
getAnalysis cfg id = do
  -- Three queries; could be one with UNIONs but separate is clearer.
  let summarySql = "SELECT id, name, source_path, captured_at::VARCHAR AS captured_at, "
                     <> "table_count, declared_fk_count, inferred_fk_count "
                     <> "FROM analyses WHERE id = " <> show id
  let fksSql = "SELECT source_table, source_columns, ref_table, ref_columns "
                 <> "FROM analysis_inferred_fks WHERE analysis_id = " <> show id
                 <> " ORDER BY source_table, source_columns"
  let proofsSql = "SELECT command_name, kind, source, verdict, interpretation, witness, scope "
                    <> "FROM analysis_proofs WHERE analysis_id = " <> show id
                    <> " ORDER BY command_name"

  summaryR <- runQuery cfg summarySql
  fksR <- runQuery cfg fksSql
  proofsR <- runQuery cfg proofsSql

  pure do
    summaries <- summaryR >>= parseSummaryArray
    case summaries of
      [] -> Right Nothing
      _ -> do
        case summaries of
          [s] -> do
            fks <- fksR >>= parseInferredFKArray
            proofs <- proofsR >>= parseProofArray
            Right $ Just { summary: s, inferredFKs: fks, proofs }
          _ -> Left "expected exactly one analysis row"

-- Parsing -----------------------------------------------------------------

parseSummaryArray :: Json -> Either String (Array AnalysisSummary)
parseSummaryArray j = do
  arr <- toArray j # note "expected JSON array"
  traverse parseSummary arr

parseSummary :: Json -> Either String AnalysisSummary
parseSummary j = do
  o <- toObject j # note "summary row not an object"
  id <- objInt o "id"
  name <- objStr o "name"
  sourcePath <- objStr o "source_path"
  capturedAt <- objStr o "captured_at"
  tableCount <- objInt o "table_count"
  declaredFKCount <- objInt o "declared_fk_count"
  inferredFKCount <- objInt o "inferred_fk_count"
  pure { id, name, sourcePath, capturedAt, tableCount, declaredFKCount, inferredFKCount }

parseInferredFKArray :: Json -> Either String (Array InferredFKRow)
parseInferredFKArray j = do
  arr <- toArray j # note "expected JSON array"
  traverse parseInferredFK arr

parseInferredFK :: Json -> Either String InferredFKRow
parseInferredFK j = do
  o <- toObject j # note "fk row not an object"
  sourceTable <- objStr o "source_table"
  sourceColumns <- objStr o "source_columns"
  refTable <- objStr o "ref_table"
  refColumns <- objStr o "ref_columns"
  pure { sourceTable, sourceColumns, refTable, refColumns }

parseProofArray :: Json -> Either String (Array ProofRow)
parseProofArray j = do
  arr <- toArray j # note "expected JSON array"
  traverse parseProof arr

parseProof :: Json -> Either String ProofRow
parseProof j = do
  o <- toObject j # note "proof row not an object"
  commandName <- objStr o "command_name"
  kind <- objStr o "kind"
  source <- objStr o "source"
  verdict <- objStr o "verdict"
  interpretation <- objStr o "interpretation"
  scope <- objInt o "scope"
  let witness = case Object.lookup "witness" o of
        Just wj | not (J.isNull wj) -> toString wj
        _ -> Nothing
  pure { commandName, kind, source, verdict, interpretation, witness, scope }

objStr :: Object.Object Json -> String -> Either String String
objStr o k =
  Object.lookup k o
    # note ("missing key: " <> k)
    >>= (\j -> toString j # note ("`" <> k <> "` not a string"))

objInt :: Object.Object Json -> String -> Either String Int
objInt o k =
  Object.lookup k o
    # note ("missing key: " <> k)
    >>= \j -> case toNumber j of
      Just n -> Right (Int.round n)
      Nothing -> case toString j >>= Int.fromString of
        Just n -> Right n
        Nothing -> Left ("`" <> k <> "` not numeric")

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg

-- | Run a SELECT and parse stdout as JSON.
runQuery :: ReadConfig -> String -> Aff (Either String Json)
runQuery cfg sql = do
  result <- spawnDuckDB cfg sql
  if result.exitCode == 0 then
    pure $ jsonParser result.stdout
  else
    pure $ Left $ "duckdb failed: " <> result.stderr

-- | Invoke `duckdb -json <db> "<sql>"` and capture stdout.
spawnDuckDB
  :: ReadConfig
  -> String
  -> Aff { exitCode :: Int, stdout :: String, stderr :: String }
spawnDuckDB cfg sql = makeAff \callback -> do
  cp <- CP.spawn cfg.duckdbBinary [ "-json", "-readonly", cfg.dbPath, sql ]
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
