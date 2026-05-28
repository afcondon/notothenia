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
import Effect.Aff.Class (liftAff)
import Foreign.Object as Object
import HTTPurple (Method(..), ServerM, badRequest, notFound, ok', serve)
import HTTPurple.Headers (ResponseHeaders, headers)
import MinardDB.Read (AnalysisDetail, AnalysisSummary, InferredFKRow, ProofRow, defaultReadConfig, getAnalysis, listAnalyses)
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
        Right (Just detail) -> ok' corsHeaders (stringify (encodeDetail detail))

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

encodeDetail :: AnalysisDetail -> Json
encodeDetail d = J.fromObject $ Object.fromFoldable
  [ "summary"     /\ encodeSummary d.summary
  , "inferredFKs" /\ J.fromArray (map encodeFK d.inferredFKs)
  , "proofs"      /\ J.fromArray (map encodeProof d.proofs)
  ]

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
  ]
