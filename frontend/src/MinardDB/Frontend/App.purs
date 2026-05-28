module MinardDB.Frontend.App where

import Prelude

import Affjax.Web as AX
import Affjax.ResponseFormat as RF
import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Core as J
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Effect.Aff.Class (class MonadAff)
import Foreign.Object as Object
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

apiBase :: String
apiBase = "http://localhost:3080"

-- Domain types (mirror backend's JSON shapes) ---

type Summary =
  { id :: Int
  , name :: String
  , sourcePath :: String
  , capturedAt :: String
  , tableCount :: Int
  , declaredFKCount :: Int
  , inferredFKCount :: Int
  }

type InferredFK =
  { sourceTable :: String
  , sourceColumns :: String
  , refTable :: String
  , refColumns :: String
  }

type Proof =
  { commandName :: String
  , kind :: String
  , source :: String
  , verdict :: String
  , interpretation :: String
  , witness :: Maybe String
  , scope :: Int
  }

type Detail =
  { summary :: Summary
  , inferredFKs :: Array InferredFK
  , proofs :: Array Proof
  }

-- Component ---

data View
  = ListView
  | DetailView Detail
  | LoadingDetail Int
  | ErrorView String

type State =
  { view :: View
  , analyses :: Array Summary
  , loading :: Boolean
  , error :: Maybe String
  }

data Action
  = Initialize
  | SelectAnalysis Int
  | BackToList
  | Refresh

component :: forall q i o m. MonadAff m => H.Component q i o m
component =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval H.defaultEval
        { handleAction = handleAction
        , initialize = Just Initialize
        }
    }

initialState :: State
initialState =
  { view: ListView
  , analyses: []
  , loading: true
  , error: Nothing
  }

handleAction :: forall o m. MonadAff m => Action -> H.HalogenM State Action () o m Unit
handleAction = case _ of
  Initialize -> loadList
  Refresh -> loadList
  BackToList -> do
    H.modify_ _ { view = ListView }
    loadList
  SelectAnalysis id -> do
    H.modify_ _ { view = LoadingDetail id }
    resp <- H.liftAff $ AX.get RF.string (apiBase <> "/api/analyses/" <> show id)
    case resp of
      Left err ->
        H.modify_ _ { view = ErrorView (AX.printError err) }
      Right r -> case jsonParser r.body >>= parseDetail of
        Left err -> H.modify_ _ { view = ErrorView err }
        Right d -> H.modify_ _ { view = DetailView d }
  where
    loadList = do
      H.modify_ _ { loading = true, error = Nothing }
      resp <- H.liftAff $ AX.get RF.string (apiBase <> "/api/analyses")
      case resp of
        Left err -> H.modify_ _ { loading = false, error = Just (AX.printError err) }
        Right r -> case jsonParser r.body >>= parseList of
          Left err -> H.modify_ _ { loading = false, error = Just err }
          Right xs -> H.modify_ _ { loading = false, analyses = xs }

-- Render ---

render :: forall m. State -> H.ComponentHTML Action () m
render state =
  HH.div [ HP.class_ (HH.ClassName "app") ]
    [ header
    , case state.view of
        ListView -> renderList state
        LoadingDetail _ -> HH.div [ HP.class_ (HH.ClassName "loading") ] [ HH.text "Loading…" ]
        DetailView d -> renderDetail d
        ErrorView err -> HH.div [ HP.class_ (HH.ClassName "error") ] [ HH.text err ]
    , footer
    ]

header :: forall a. HH.HTML a Action
header =
  HH.header [ HP.class_ (HH.ClassName "site-header") ]
    [ HH.h1_ [ HH.text "Notothenia" ]
    , HH.p [ HP.class_ (HH.ClassName "subtitle") ]
        [ HH.text "Minard-DB · schema cartography with Alloy-backed proofs" ]
    ]

footer :: forall a. HH.HTML a Action
footer =
  HH.footer [ HP.class_ (HH.ClassName "site-footer") ]
    [ HH.text "“Cod with only one ’d’.” — a tribute to E.F. Codd." ]

renderList :: forall m. State -> H.ComponentHTML Action () m
renderList state =
  HH.section [ HP.class_ (HH.ClassName "list-view") ]
    [ HH.h2_ [ HH.text "Analyses" ]
    , case state.error of
        Just err -> HH.div [ HP.class_ (HH.ClassName "error") ] [ HH.text err ]
        Nothing ->
          if state.loading then
            HH.div [ HP.class_ (HH.ClassName "loading") ] [ HH.text "Loading…" ]
          else if Array.null state.analyses then
            HH.div [ HP.class_ (HH.ClassName "empty") ]
              [ HH.text "No analyses yet. Run "
              , HH.code_ [ HH.text "spago run -p minard-db --main MinardDB.Analyze -- <schema.json>" ]
              ]
          else
            HH.div [ HP.class_ (HH.ClassName "analysis-grid") ]
              (map analysisCard state.analyses)
    ]

analysisCard :: forall m. Summary -> H.ComponentHTML Action () m
analysisCard s =
  HH.article
    [ HP.class_ (HH.ClassName "analysis-card")
    , HE.onClick (\_ -> SelectAnalysis s.id)
    ]
    [ HH.h3_ [ HH.text s.name ]
    , HH.p [ HP.class_ (HH.ClassName "source-path") ]
        [ HH.code_ [ HH.text s.sourcePath ] ]
    , HH.div [ HP.class_ (HH.ClassName "stats") ]
        [ stat "tables" s.tableCount
        , stat "declared FKs" s.declaredFKCount
        , stat "inferred FKs" s.inferredFKCount
        ]
    , HH.p [ HP.class_ (HH.ClassName "timestamp") ]
        [ HH.text s.capturedAt ]
    ]

stat :: forall a. String -> Int -> HH.HTML a Action
stat label value =
  HH.div [ HP.class_ (HH.ClassName "stat") ]
    [ HH.span [ HP.class_ (HH.ClassName "stat-value") ] [ HH.text (show value) ]
    , HH.span [ HP.class_ (HH.ClassName "stat-label") ] [ HH.text label ]
    ]

renderDetail :: forall m. Detail -> H.ComponentHTML Action () m
renderDetail d =
  HH.section [ HP.class_ (HH.ClassName "detail-view") ]
    [ HH.div [ HP.class_ (HH.ClassName "breadcrumb") ]
        [ HH.a [ HP.href "#", HE.onClick (\_ -> BackToList) ]
            [ HH.text "← All analyses" ]
        ]
    , HH.h2_ [ HH.text d.summary.name ]
    , HH.p [ HP.class_ (HH.ClassName "source-path") ]
        [ HH.code_ [ HH.text d.summary.sourcePath ]
        , HH.text " · "
        , HH.text d.summary.capturedAt
        ]
    , HH.div [ HP.class_ (HH.ClassName "stats-row") ]
        [ stat "tables" d.summary.tableCount
        , stat "declared FKs" d.summary.declaredFKCount
        , stat "inferred FKs" d.summary.inferredFKCount
        ]
    , noDeclaredFKsWarning d
    , inferredFKsSection d.inferredFKs
    , proofsSection d.proofs
    ]

noDeclaredFKsWarning :: forall a. Detail -> HH.HTML a Action
noDeclaredFKsWarning d =
  if d.summary.declaredFKCount == 0 && d.summary.inferredFKCount > 0 then
    HH.div [ HP.class_ (HH.ClassName "warning") ]
      [ HH.h3_ [ HH.text "⚠  No declared foreign key constraints" ]
      , HH.p_
          [ HH.text "This schema has "
          , HH.strong_ [ HH.text (show d.summary.tableCount <> " tables") ]
          , HH.text " but "
          , HH.strong_ [ HH.text "zero" ]
          , HH.text " declared FK constraints. Referential integrity is entirely application-mediated."
          ]
      , HH.p_
          [ HH.text "Below are "
          , HH.strong_ [ HH.text (show d.summary.inferredFKCount) ]
          , HH.text " candidate FKs inferred from "
          , HH.code_ [ HH.text "<entity>_id" ]
          , HH.text " column-naming conventions. The proof results show what would hold "
          , HH.em_ [ HH.text "if" ]
          , HH.text " these were declared."
          ]
      ]
  else
    HH.text ""

inferredFKsSection :: forall a. Array InferredFK -> HH.HTML a Action
inferredFKsSection fks =
  if Array.null fks then
    HH.text ""
  else
    HH.section [ HP.class_ (HH.ClassName "fk-section") ]
      [ HH.h3_ [ HH.text "Inferred foreign keys" ]
      , HH.table_
          [ HH.thead_
              [ HH.tr_
                  [ HH.th_ [ HH.text "Source table" ]
                  , HH.th_ [ HH.text "Source columns" ]
                  , HH.th_ [ HH.text "Refers to" ]
                  , HH.th_ [ HH.text "Ref columns" ]
                  ]
              ]
          , HH.tbody_ (map fkRow fks)
          ]
      ]

fkRow :: forall a. InferredFK -> HH.HTML a Action
fkRow fk =
  HH.tr_
    [ HH.td_ [ HH.code_ [ HH.text fk.sourceTable ] ]
    , HH.td_ [ HH.code_ [ HH.text fk.sourceColumns ] ]
    , HH.td_ [ HH.code_ [ HH.text fk.refTable ] ]
    , HH.td_ [ HH.code_ [ HH.text fk.refColumns ] ]
    ]

proofsSection :: forall a. Array Proof -> HH.HTML a Action
proofsSection proofs =
  HH.section [ HP.class_ (HH.ClassName "proofs-section") ]
    [ HH.h3_ [ HH.text "Proofs" ]
    , HH.table_
        [ HH.thead_
            [ HH.tr_
                [ HH.th_ [ HH.text "Property" ]
                , HH.th_ [ HH.text "Kind" ]
                , HH.th_ [ HH.text "Scope" ]
                , HH.th_ [ HH.text "Verdict" ]
                , HH.th_ [ HH.text "Interpretation" ]
                , HH.th_ [ HH.text "Witness" ]
                ]
            ]
        , HH.tbody_ (map proofRow proofs)
        ]
    ]

proofRow :: forall a. Proof -> HH.HTML a Action
proofRow p =
  HH.tr [ HP.class_ (HH.ClassName ("proof-row " <> verdictClass p)) ]
    [ HH.td_ [ HH.code_ [ HH.text p.commandName ] ]
    , HH.td_ [ HH.text p.kind ]
    , HH.td_ [ HH.text (show p.scope) ]
    , HH.td [ HP.class_ (HH.ClassName "verdict") ]
        [ HH.text p.verdict ]
    , HH.td_ [ HH.text p.interpretation ]
    , HH.td_ [ case p.witness of
        Just w -> HH.code_ [ HH.text w ]
        Nothing -> HH.text "—"
      ]
    ]

verdictClass :: Proof -> String
verdictClass p = case p.kind, p.verdict of
  "check", "UNSAT" -> "ok"
  "check", "SAT" -> "broken"
  "run", "SAT" -> "ok"
  _, _ -> "neutral"

-- JSON parsing ---

parseList :: Json -> Either String (Array Summary)
parseList j = do
  obj <- toObject j # note "list root is not an object"
  arrJ <- Object.lookup "analyses" obj # note "missing analyses key"
  arr <- toArray arrJ # note "analyses is not an array"
  traverse parseSummary arr

parseSummary :: Json -> Either String Summary
parseSummary j = do
  obj <- toObject j # note "summary is not an object"
  id <- objInt obj "id"
  name <- objStr obj "name"
  sourcePath <- objStr obj "sourcePath"
  capturedAt <- objStr obj "capturedAt"
  tableCount <- objInt obj "tableCount"
  declaredFKCount <- objInt obj "declaredFKCount"
  inferredFKCount <- objInt obj "inferredFKCount"
  pure { id, name, sourcePath, capturedAt, tableCount, declaredFKCount, inferredFKCount }

parseDetail :: Json -> Either String Detail
parseDetail j = do
  obj <- toObject j # note "detail is not an object"
  sumJ <- Object.lookup "summary" obj # note "missing summary"
  summary <- parseSummary sumJ
  fksJ <- Object.lookup "inferredFKs" obj # note "missing inferredFKs"
  fksArr <- toArray fksJ # note "inferredFKs not an array"
  inferredFKs <- traverse parseFK fksArr
  proofsJ <- Object.lookup "proofs" obj # note "missing proofs"
  proofsArr <- toArray proofsJ # note "proofs not an array"
  proofs <- traverse parseProof proofsArr
  pure { summary, inferredFKs, proofs }

parseFK :: Json -> Either String InferredFK
parseFK j = do
  obj <- toObject j # note "fk not an object"
  sourceTable <- objStr obj "sourceTable"
  sourceColumns <- objStr obj "sourceColumns"
  refTable <- objStr obj "refTable"
  refColumns <- objStr obj "refColumns"
  pure { sourceTable, sourceColumns, refTable, refColumns }

parseProof :: Json -> Either String Proof
parseProof j = do
  obj <- toObject j # note "proof not an object"
  commandName <- objStr obj "commandName"
  kind <- objStr obj "kind"
  source <- objStr obj "source"
  verdict <- objStr obj "verdict"
  interpretation <- objStr obj "interpretation"
  scope <- objInt obj "scope"
  let witness = case Object.lookup "witness" obj of
        Just wj | not (J.isNull wj) -> toString wj
        _ -> Nothing
  pure { commandName, kind, source, verdict, interpretation, witness, scope }

objStr :: Object.Object Json -> String -> Either String String
objStr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\j -> toString j # note (k <> " not a string"))

objInt :: Object.Object Json -> String -> Either String Int
objInt o k = Object.lookup k o # note ("missing " <> k)
  >>= \j -> case toNumber j of
    Just n -> Right (Int.round n)
    Nothing -> Left (k <> " not a number")

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg
