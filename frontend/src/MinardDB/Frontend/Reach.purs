-- | Query reach view (Phase 4b).
-- |
-- | Renders a reach report (produced offline by
-- | `MinardDB.Query.Report`, served at `/api/reach`) as a schema usage
-- | heatmap: a grid of table cards, each column drawn with a bar
-- | proportional to how many queries touch it. Dead columns — those no
-- | query reaches — are flagged. Below the grid, each query's footprint
-- | (the tables and columns it touches, plus any ambiguous or
-- | unresolved references) is listed.
-- |
-- | The headline the view exists to deliver: dead columns are
-- | immediately visible (a hollow bar + DEAD tag), and because the
-- | parser errs toward over-collecting references, a column flagged
-- | dead here is genuinely unreferenced by every query in the set — a
-- | real candidate for DROP COLUMN (which Phase 3 can then verify
-- | preserves referential integrity).
module MinardDB.Frontend.Reach
  ( ReachReport
  , TableUsage
  , ColumnUsage
  , QueryFootprint
  , parseReachReports
  , reachList
  , reachView
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

------------------------------------------------------------------------
-- Model (mirrors the backend JSON)
------------------------------------------------------------------------

type ColumnUsage = { name :: String, reachCount :: Int, dead :: Boolean }
type TableUsage = { name :: String, columns :: Array ColumnUsage }

type QueryFootprint =
  { label :: String
  , kind :: String
  , sql :: String
  , tables :: Array String
  , touched :: Array String
  , ambiguous :: Array String
  , unresolved :: Array String
  , parseError :: Maybe String
  }

type ReachReport =
  { name :: String
  , blurb :: String
  , schemaName :: String
  , totalColumns :: Int
  , reachedCount :: Int
  , deadCount :: Int
  , tables :: Array TableUsage
  , queries :: Array QueryFootprint
  }

------------------------------------------------------------------------
-- List view
------------------------------------------------------------------------

reachList :: forall w i. Array ReachReport -> (String -> i) -> HH.HTML w i
reachList reports onSelect =
  HH.section [ HP.class_ (HH.ClassName "list-view") ]
    [ HH.h2_ [ HH.text "Query reach" ]
    , HH.p [ HP.class_ (HH.ClassName "subtitle") ]
        [ HH.text "Which columns the queries touch, and which are dead — referenced by nothing. A dead column is a candidate for DROP COLUMN, which the migration verifier can then prove safe." ]
    , if Array.null reports then
        HH.div [ HP.class_ (HH.ClassName "empty") ]
          [ HH.text "No reach reports yet. Run "
          , HH.code_ [ HH.text "spago run -p minard-db --main MinardDB.Query.Report" ]
          ]
      else
        HH.div [ HP.class_ (HH.ClassName "analysis-grid") ]
          (map (reportCard onSelect) reports)
    ]

reportCard :: forall w i. (String -> i) -> ReachReport -> HH.HTML w i
reportCard onSelect r =
  HH.article
    [ HP.class_ (HH.ClassName "analysis-card")
    , HE.onClick (\_ -> onSelect r.name)
    ]
    [ HH.h3_ [ HH.text r.name ]
    , HH.p [ HP.class_ (HH.ClassName "source-path") ] [ HH.text r.blurb ]
    , HH.div [ HP.class_ (HH.ClassName "stats") ]
        [ stat "tables" (Array.length r.tables)
        , stat "reached" r.reachedCount
        , stat "dead" r.deadCount
        ]
    ]

stat :: forall w i. String -> Int -> HH.HTML w i
stat label value =
  HH.div [ HP.class_ (HH.ClassName "stat") ]
    [ HH.span [ HP.class_ (HH.ClassName "stat-value") ] [ HH.text (show value) ]
    , HH.span [ HP.class_ (HH.ClassName "stat-label") ] [ HH.text label ]
    ]

------------------------------------------------------------------------
-- Detail view (the usage heatmap grid + query footprints)
------------------------------------------------------------------------

reachView :: forall w i. ReachReport -> i -> HH.HTML w i
reachView r onBack =
  let
    maxCount = foldl max 1 (map _.reachCount (Array.concatMap _.columns r.tables))
  in
    HH.section [ HP.class_ (HH.ClassName "detail-view") ]
      [ HH.div [ HP.class_ (HH.ClassName "breadcrumb") ]
          [ HH.a [ HP.href "#r", HE.onClick (\_ -> onBack) ]
              [ HH.text "← All reach reports" ]
          ]
      , HH.h2_ [ HH.text r.name ]
      , HH.p [ HP.class_ (HH.ClassName "source-path") ] [ HH.text r.blurb ]
      , HH.div [ HP.class_ (HH.ClassName "stats-row") ]
          [ stat "tables" (Array.length r.tables)
          , stat "reached" r.reachedCount
          , stat "dead" r.deadCount
          , stat "columns" r.totalColumns
          ]
      , deadCallout r
      , HH.h3_ [ HH.text "Schema usage" ]
      , HH.div [ HP.class_ (HH.ClassName "reach-grid") ]
          (map (tableCard maxCount) r.tables)
      , HH.h3_ [ HH.text "Queries" ]
      , HH.div [ HP.class_ (HH.ClassName "footprints") ]
          (map footprint r.queries)
      ]

deadCallout :: forall w i. ReachReport -> HH.HTML w i
deadCallout r =
  if r.deadCount == 0 then
    HH.div [ HP.class_ (HH.ClassName "transit-callout clean") ]
      [ HH.strong_ [ HH.text "No dead columns." ]
      , HH.text " Every column is referenced by at least one query." ]
  else
    HH.div [ HP.class_ (HH.ClassName "transit-callout") ]
      [ HH.strong_ [ HH.text (show r.deadCount <> " dead column(s).") ]
      , HH.text " Referenced by no query in this set — candidates for DROP COLUMN. The migration verifier can prove the drop preserves referential integrity before you commit to it." ]

tableCard :: forall w i. Int -> TableUsage -> HH.HTML w i
tableCard maxCount t =
  HH.div [ HP.class_ (HH.ClassName "reach-table") ]
    [ HH.div [ HP.class_ (HH.ClassName "reach-table-name") ] [ HH.text t.name ]
    , HH.div [ HP.class_ (HH.ClassName "reach-cols") ]
        (map (columnRow maxCount) t.columns)
    ]

columnRow :: forall w i. Int -> ColumnUsage -> HH.HTML w i
columnRow maxCount c =
  let
    pct = (Int.toNumber c.reachCount / Int.toNumber maxCount) * 100.0
    rowCls = "reach-col" <> (if c.dead then " dead" else "")
  in
    HH.div [ HP.class_ (HH.ClassName rowCls) ]
      [ HH.span [ HP.class_ (HH.ClassName "reach-col-name") ] [ HH.text c.name ]
      , HH.span [ HP.class_ (HH.ClassName "reach-bar-track") ]
          [ HH.span
              [ HP.class_ (HH.ClassName "reach-bar")
              , HP.style ("width:" <> show pct <> "%")
              ]
              []
          ]
      , if c.dead then
          HH.span [ HP.class_ (HH.ClassName "reach-dead-tag") ] [ HH.text "DEAD" ]
        else
          HH.span [ HP.class_ (HH.ClassName "reach-count") ] [ HH.text (show c.reachCount) ]
      ]

footprint :: forall w i. QueryFootprint -> HH.HTML w i
footprint q =
  HH.div [ HP.class_ (HH.ClassName "footprint") ]
    [ HH.div [ HP.class_ (HH.ClassName "footprint-head") ]
        [ HH.span [ HP.class_ (HH.ClassName "footprint-kind") ] [ HH.text q.kind ]
        , HH.span [ HP.class_ (HH.ClassName "footprint-label") ] [ HH.text q.label ]
        ]
    , HH.pre [ HP.class_ (HH.ClassName "footprint-sql") ] [ HH.code_ [ HH.text q.sql ] ]
    , case q.parseError of
        Just err ->
          HH.div [ HP.class_ (HH.ClassName "footprint-warn") ]
            [ HH.text ("⚠ parse error: " <> err) ]
        Nothing ->
          HH.div [ HP.class_ (HH.ClassName "footprint-reach") ]
            [ labeled "reads" (Array.intercalate ", " q.tables)
            , labeled "touches" (Array.intercalate ", " q.touched)
            , warnRow "ambiguous" q.ambiguous
            , warnRow "unresolved" q.unresolved
            ]
    ]

labeled :: forall w i. String -> String -> HH.HTML w i
labeled label value =
  HH.div [ HP.class_ (HH.ClassName "fp-line") ]
    [ HH.span [ HP.class_ (HH.ClassName "fp-label") ] [ HH.text label ]
    , HH.span_ [ HH.text (if value == "" then "—" else value) ]
    ]

warnRow :: forall w i. String -> Array String -> HH.HTML w i
warnRow label xs =
  if Array.null xs then HH.text ""
  else
    HH.div [ HP.class_ (HH.ClassName "fp-line warn") ]
      [ HH.span [ HP.class_ (HH.ClassName "fp-label") ] [ HH.text label ]
      , HH.span_ [ HH.text (Array.intercalate ", " xs) ]
      ]

------------------------------------------------------------------------
-- JSON parsing
------------------------------------------------------------------------

parseReachReports :: Json -> Either String (Array ReachReport)
parseReachReports j = do
  obj <- toObject j # note "reach root not an object"
  arr <- objArr obj "reach"
  traverse parseReport arr

parseReport :: Json -> Either String ReachReport
parseReport j = do
  obj <- toObject j # note "report not an object"
  name <- objStr obj "name"
  blurb <- objStr obj "blurb"
  schemaName <- objStr obj "schemaName"
  totalColumns <- objInt obj "totalColumns"
  reachedCount <- objInt obj "reachedCount"
  deadCount <- objInt obj "deadCount"
  tablesJ <- objArr obj "tables"
  tables <- traverse parseTable tablesJ
  queriesJ <- objArr obj "queries"
  queries <- traverse parseFootprint queriesJ
  pure { name, blurb, schemaName, totalColumns, reachedCount, deadCount, tables, queries }

parseTable :: Json -> Either String TableUsage
parseTable j = do
  obj <- toObject j # note "table not an object"
  name <- objStr obj "name"
  colsJ <- objArr obj "columns"
  columns <- traverse parseColumn colsJ
  pure { name, columns }

parseColumn :: Json -> Either String ColumnUsage
parseColumn j = do
  obj <- toObject j # note "column not an object"
  name <- objStr obj "name"
  reachCount <- objInt obj "reachCount"
  dead <- objBool obj "dead"
  pure { name, reachCount, dead }

parseFootprint :: Json -> Either String QueryFootprint
parseFootprint j = do
  obj <- toObject j # note "query not an object"
  label <- objStr obj "label"
  kind <- objStr obj "kind"
  sql <- objStr obj "sql"
  tables <- objStrArr obj "tables"
  touched <- objStrArr obj "touched"
  ambiguous <- objStrArr obj "ambiguous"
  unresolved <- objStrArr obj "unresolved"
  let parseError = case Object.lookup "parseError" obj of
        Just pj | not (J.isNull pj) -> toString pj
        _ -> Nothing
  pure { label, kind, sql, tables, touched, ambiguous, unresolved, parseError }

objStr :: Object Json -> String -> Either String String
objStr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> toString v # note (k <> " not a string"))

objInt :: Object Json -> String -> Either String Int
objInt o k = Object.lookup k o # note ("missing " <> k)
  >>= \v -> case toNumber v of
    Just n -> Right (Int.round n)
    Nothing -> Left (k <> " not numeric")

objBool :: Object Json -> String -> Either String Boolean
objBool o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> J.toBoolean v # note (k <> " not a boolean"))

objArr :: Object Json -> String -> Either String (Array Json)
objArr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> toArray v # note (k <> " not an array"))

objStrArr :: Object Json -> String -> Either String (Array String)
objStrArr o k = do
  arr <- objArr o k
  traverse (\v -> toString v # note (k <> " element not a string")) arr

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg
