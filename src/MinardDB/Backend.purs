module MinardDB.Backend where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Either (Either(..))
import Data.Generic.Rep (class Generic)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested ((/\))
import Effect.Aff (Aff, attempt)
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import HTTPurple (Method(..), ServerM, badRequest, notFound, ok', serve)
import HTTPurple.Headers (ResponseHeaders, headers)
import MinardDB.Read (AnalysisDetail, AnalysisSummary, InferredFKRow, ProofRow, defaultReadConfig, getAnalysis, listAnalyses)
import MinardDB.Schema (FKAction, ForeignKey, Schema, Table)
import MinardDB.Schema.JSON (parseSchemaFull)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Routing.Duplex (RouteDuplex', int, path, root, segment)
import Routing.Duplex.Generic (noArgs, sum)

data Route
  = Health
  | ListAnalyses
  | GetAnalysis Int

derive instance genericRoute :: Generic Route _

routes :: RouteDuplex' Route
routes = root $ sum
  { "Health":       path "health" noArgs
  , "ListAnalyses": path "api" (path "analyses" noArgs)
  , "GetAnalysis":  path "api" (path "analyses" (int segment))
  }

corsHeaders :: ResponseHeaders
corsHeaders = headers
  { "Access-Control-Allow-Origin": "*"
  , "Access-Control-Allow-Methods": "GET, OPTIONS"
  , "Access-Control-Allow-Headers": "Content-Type"
  , "Content-Type": "application/json"
  }

main :: ServerM
main = serve { port: 3080, hostname: "localhost" } { route: routes, router }
  where
    router { route, method } = case method of
      Options -> ok' corsHeaders ""
      _ -> handle route

    handle Health = ok' corsHeaders "{\"status\":\"ok\"}"

    handle ListAnalyses = do
      result <- liftAff $ listAnalyses defaultReadConfig
      case result of
        Left err -> badRequest err
        Right xs -> ok' corsHeaders (stringify (encodeListResponse xs))

    handle (GetAnalysis id) = do
      result <- liftAff $ getAnalysis defaultReadConfig id
      case result of
        Left err -> badRequest err
        Right Nothing -> notFound
        Right (Just detail) -> do
          mschema <- liftAff $ tryReadSchema detail.summary.sourcePath
          ok' corsHeaders (stringify (encodeDetail detail mschema))

-- JSON encoders --------------------------------------------------------------

encodeListResponse :: Array AnalysisSummary -> Json
encodeListResponse xs = J.fromObject $ Object.fromFoldable
  [ "analyses" /\ J.fromArray (map encodeSummary xs)
  , "count"    /\ J.fromNumber (Int.toNumber (Array.length xs))
  ]

encodeSummary :: AnalysisSummary -> Json
encodeSummary s = J.fromObject $ Object.fromFoldable
  [ "id"              /\ J.fromNumber (Int.toNumber s.id)
  , "name"            /\ J.fromString s.name
  , "sourcePath"      /\ J.fromString s.sourcePath
  , "capturedAt"      /\ J.fromString s.capturedAt
  , "tableCount"      /\ J.fromNumber (Int.toNumber s.tableCount)
  , "declaredFKCount" /\ J.fromNumber (Int.toNumber s.declaredFKCount)
  , "inferredFKCount" /\ J.fromNumber (Int.toNumber s.inferredFKCount)
  ]

encodeDetail :: AnalysisDetail -> Maybe Schema -> Json
encodeDetail d mschema = J.fromObject $ Object.fromFoldable
  [ "summary"     /\ encodeSummary d.summary
  , "inferredFKs" /\ J.fromArray (map encodeFK d.inferredFKs)
  , "proofs"      /\ J.fromArray (map encodeProof d.proofs)
  , "schema"      /\ case mschema of
      Just s  -> encodeSchema s
      Nothing -> J.jsonNull
  ]

-- | Encode a parsed Schema for the topology view: a list of tables
-- | (name + columnCount + pkColumns) and a flat list of FKs (source
-- | table + column(s) + ref table + onDelete). Inferred FKs are merged
-- | in via parseSchemaFull's `withInferred` view so the topology shows
-- | the same FK graph the proof catalog ran against.
encodeSchema :: Schema -> Json
encodeSchema s = J.fromObject $ Object.fromFoldable
  [ "name"   /\ J.fromString s.name
  , "tables" /\ J.fromArray (map encodeTable s.tables)
  , "fks"    /\ J.fromArray (Array.concatMap tableFKs s.tables)
  ]
  where
    tableFKs :: Table -> Array Json
    tableFKs t = map (encodeFKEdge t.name) t.foreignKeys

encodeTable :: Table -> Json
encodeTable t = J.fromObject $ Object.fromFoldable
  [ "name"        /\ J.fromString t.name
  , "columnCount" /\ J.fromNumber (Int.toNumber (Array.length t.columns))
  , "primaryKey"  /\ J.fromArray (map J.fromString t.primaryKey)
  ]

encodeFKEdge :: String -> ForeignKey -> Json
encodeFKEdge srcTable fk = J.fromObject $ Object.fromFoldable
  [ "sourceTable"   /\ J.fromString srcTable
  , "sourceColumns" /\ J.fromArray (map J.fromString fk.columns)
  , "refTable"      /\ J.fromString fk.refTable
  , "refColumns"    /\ J.fromArray (map J.fromString fk.refColumns)
  , "onDelete"      /\ J.fromString (showFKAction fk.onDelete)
  ]

showFKAction :: FKAction -> String
showFKAction = show

-- | Best-effort schema read for the topology view. We merge inferred FKs
-- | into the FK list (matching what the proof catalog actually ran
-- | against). If the source file has moved or is unreadable we return
-- | Nothing and the frontend gracefully degrades — better than 500ing
-- | the whole detail endpoint over a missing fixture.
tryReadSchema :: String -> Aff (Maybe Schema)
tryReadSchema path = do
  attempt (FS.readTextFile UTF8 path) >>= case _ of
    Left _ -> pure Nothing
    Right text -> case parseSchemaFull text of
      Left _ -> pure Nothing
      Right parsed -> pure (Just parsed.withInferred)

encodeFK :: InferredFKRow -> Json
encodeFK fk = J.fromObject $ Object.fromFoldable
  [ "sourceTable"   /\ J.fromString fk.sourceTable
  , "sourceColumns" /\ J.fromString fk.sourceColumns
  , "refTable"      /\ J.fromString fk.refTable
  , "refColumns"    /\ J.fromString fk.refColumns
  ]

encodeProof :: ProofRow -> Json
encodeProof p = J.fromObject $ Object.fromFoldable
  [ "commandName"    /\ J.fromString p.commandName
  , "kind"           /\ J.fromString p.kind
  , "source"         /\ J.fromString p.source
  , "verdict"        /\ J.fromString p.verdict
  , "interpretation" /\ J.fromString p.interpretation
  , "witness"        /\ case p.witness of
      Just w -> J.fromString w
      Nothing -> J.jsonNull
  , "scope"          /\ J.fromNumber (Int.toNumber p.scope)
  , "minScope"       /\ case p.minScope of
      Just n -> J.fromNumber (Int.toNumber n)
      Nothing -> J.jsonNull
  ]
