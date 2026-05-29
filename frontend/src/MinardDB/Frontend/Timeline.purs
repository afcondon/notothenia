-- | Migration timeline view (Phase 3e).
-- |
-- | Renders a `MigrationReport` (produced offline by
-- | `MinardDB.Migration.Report`, served by the backend at
-- | `/api/migrations`) as a vertical timeline of steps. Each step is a
-- | row on a left-hand rail; the rail dot is green when referential
-- | integrity holds *after* that step and red when it doesn't.
-- |
-- | The story the view exists to tell: a migration can end in a
-- | perfectly consistent state (final standing count = 0) while passing
-- | through an inconsistent one mid-flight (a step whose standing count
-- | spiked above zero). That "broken in transit" case is called out
-- | explicitly — it's the argument for verifying the whole trace
-- | temporally rather than just diffing the endpoints.
module MinardDB.Frontend.Timeline
  ( MigrationReport
  , StepCell
  , parseReports
  , timelineList
  , timelineView
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
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

type StepCell =
  { index :: Int
  , description :: String
  , introduced :: Array String
  , resolved :: Array String
  , standingAfter :: Int
  }

type MigrationReport =
  { name :: String
  , blurb :: String
  , schemaVerdict :: String       -- PROVEN | BROKEN | UNKNOWN
  , rowVerdict :: String
  , totalIntroduced :: Int
  , finalStandingCount :: Int
  , steps :: Array StepCell
  }

------------------------------------------------------------------------
-- List view (cards, one per report)
------------------------------------------------------------------------

timelineList :: forall w i. Array MigrationReport -> (String -> i) -> HH.HTML w i
timelineList reports onSelect =
  HH.section [ HP.class_ (HH.ClassName "list-view") ]
    [ HH.h2_ [ HH.text "Migrations" ]
    , HH.p [ HP.class_ (HH.ClassName "subtitle") ]
        [ HH.text "Each report runs an ordered migration sequence through the static safety pass and a bounded Alloy 6 temporal check. RI is verified across the whole trace, not just at the endpoints." ]
    , if Array.null reports then
        HH.div [ HP.class_ (HH.ClassName "empty") ]
          [ HH.text "No migration reports yet. Run "
          , HH.code_ [ HH.text "spago run -p minard-db --main MinardDB.Migration.Report" ]
          ]
      else
        HH.div [ HP.class_ (HH.ClassName "analysis-grid") ]
          (map (reportCard onSelect) reports)
    ]

reportCard :: forall w i. (String -> i) -> MigrationReport -> HH.HTML w i
reportCard onSelect r =
  HH.article
    [ HP.class_ (HH.ClassName "analysis-card")
    , HE.onClick (\_ -> onSelect r.name)
    ]
    [ HH.h3_ [ HH.text r.name ]
    , HH.p [ HP.class_ (HH.ClassName "source-path") ] [ HH.text r.blurb ]
    , HH.div [ HP.class_ (HH.ClassName "verdict-badges") ]
        [ verdictBadge "schema RI" r.schemaVerdict
        , verdictBadge "row RI" r.rowVerdict
        ]
    , HH.p [ HP.class_ (HH.ClassName "timestamp") ]
        [ HH.text (show (Array.length r.steps) <> " steps") ]
    ]

------------------------------------------------------------------------
-- Detail view (the vertical timeline)
------------------------------------------------------------------------

timelineView :: forall w i. MigrationReport -> i -> HH.HTML w i
timelineView r onBack =
  HH.section [ HP.class_ (HH.ClassName "detail-view") ]
    [ HH.div [ HP.class_ (HH.ClassName "breadcrumb") ]
        [ HH.a [ HP.href "#m", HE.onClick (\_ -> onBack) ]
            [ HH.text "← All migrations" ]
        ]
    , HH.h2_ [ HH.text r.name ]
    , HH.p [ HP.class_ (HH.ClassName "source-path") ] [ HH.text r.blurb ]
    , HH.div [ HP.class_ (HH.ClassName "verdict-badges verdict-badges-lg") ]
        [ verdictBadge "schema RI" r.schemaVerdict
        , verdictBadge "row RI" r.rowVerdict
        ]
    , transitCallout r
    , HH.ol [ HP.class_ (HH.ClassName "timeline") ]
        (map stepRow r.steps)
    ]

-- | When the migration ends clean (0 standing) but some step spiked
-- | above zero, surface that contrast prominently — it's the whole
-- | point of doing this in temporal logic.
transitCallout :: forall w i. MigrationReport -> HH.HTML w i
transitCallout r =
  let
    maxStanding = foldl (\m s -> max m s.standingAfter) 0 r.steps
    brokenStep = Array.find (\s -> s.standingAfter > 0) r.steps
  in
    if r.finalStandingCount == 0 && maxStanding > 0 then
      case brokenStep of
        Just s ->
          HH.div [ HP.class_ (HH.ClassName "transit-callout") ]
            [ HH.strong_ [ HH.text "Broken in transit." ]
            , HH.text " This migration ends in a consistent state — "
            , HH.strong_ [ HH.text "zero" ]
            , HH.text " standing issues — yet referential integrity is violated mid-sequence, first at step "
            , HH.strong_ [ HH.text (show (s.index + 1)) ]
            , HH.text " ("
            , HH.code_ [ HH.text s.description ]
            , HH.text "). An endpoint-only check would call this migration clean; the temporal check does not."
            ]
        Nothing -> HH.text ""
    else if r.finalStandingCount > 0 then
      HH.div [ HP.class_ (HH.ClassName "transit-callout broken") ]
        [ HH.strong_ [ HH.text "Ends broken." ]
        , HH.text " "
        , HH.text (show r.finalStandingCount)
        , HH.text " dangling foreign key(s) remain after the final step."
        ]
    else
      HH.div [ HP.class_ (HH.ClassName "transit-callout clean") ]
        [ HH.strong_ [ HH.text "Clean throughout." ]
        , HH.text " Referential integrity holds at every step of the sequence."
        ]

stepRow :: forall w i. StepCell -> HH.HTML w i
stepRow s =
  let
    broken = s.standingAfter > 0
    cls = "tl-step " <> (if broken then "broken" else "clean")
  in
    HH.li [ HP.class_ (HH.ClassName cls) ]
      [ HH.div [ HP.class_ (HH.ClassName "tl-rail") ]
          [ HH.span [ HP.class_ (HH.ClassName "tl-dot") ] [] ]
      , HH.div [ HP.class_ (HH.ClassName "tl-body") ]
          [ HH.div [ HP.class_ (HH.ClassName "tl-head") ]
              [ HH.span [ HP.class_ (HH.ClassName "tl-num") ] [ HH.text (show (s.index + 1)) ]
              , HH.code [ HP.class_ (HH.ClassName "tl-desc") ] [ HH.text s.description ]
              , if broken then
                  HH.span [ HP.class_ (HH.ClassName "tl-standing") ]
                    [ HH.text (show s.standingAfter <> " dangling") ]
                else
                  HH.text ""
              ]
          , issueList "introduced" s.introduced
          , issueList "resolved" s.resolved
          ]
      ]

issueList :: forall w i. String -> Array String -> HH.HTML w i
issueList kind issues =
  if Array.null issues then
    HH.text ""
  else
    HH.ul [ HP.class_ (HH.ClassName ("tl-issues " <> kind)) ]
      (map (\msg -> HH.li_ [ HH.text (marker <> msg) ]) issues)
  where
  marker = case kind of
    "introduced" -> "⚠ "
    "resolved" -> "✓ "
    _ -> "• "

verdictBadge :: forall w i. String -> String -> HH.HTML w i
verdictBadge label verdict =
  HH.span [ HP.class_ (HH.ClassName ("verdict-badge " <> verdictClass verdict)) ]
    [ HH.span [ HP.class_ (HH.ClassName "vb-label") ] [ HH.text label ]
    , HH.span [ HP.class_ (HH.ClassName "vb-value") ] [ HH.text verdict ]
    ]

verdictClass :: String -> String
verdictClass = case _ of
  "PROVEN" -> "ok"
  "BROKEN" -> "broken"
  _ -> "neutral"

------------------------------------------------------------------------
-- JSON parsing
------------------------------------------------------------------------

-- | Parse the `{ "migrations": [ … ] }` envelope from /api/migrations.
parseReports :: Json -> Either String (Array MigrationReport)
parseReports j = do
  obj <- toObject j # note "migrations root not an object"
  arrJ <- objArr obj "migrations"
  traverse parseReport arrJ

parseReport :: Json -> Either String MigrationReport
parseReport j = do
  obj <- toObject j # note "report not an object"
  name <- objStr obj "name"
  blurb <- objStr obj "blurb"
  schemaVerdict <- objStr obj "schemaVerdict"
  rowVerdict <- objStr obj "rowVerdict"
  totalIntroduced <- objInt obj "totalIntroduced"
  finalStandingCount <- objInt obj "finalStandingCount"
  stepsJ <- objArr obj "steps"
  steps <- traverse parseStep stepsJ
  pure { name, blurb, schemaVerdict, rowVerdict, totalIntroduced, finalStandingCount, steps }

parseStep :: Json -> Either String StepCell
parseStep j = do
  obj <- toObject j # note "step not an object"
  index <- objInt obj "index"
  description <- objStr obj "description"
  introduced <- objStrArr obj "introduced"
  resolved <- objStrArr obj "resolved"
  standingAfter <- objInt obj "standingAfter"
  pure { index, description, introduced, resolved, standingAfter }

objStr :: Object Json -> String -> Either String String
objStr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> toString v # note (k <> " not a string"))

objInt :: Object Json -> String -> Either String Int
objInt o k = Object.lookup k o # note ("missing " <> k)
  >>= \v -> case toNumber v of
    Just n -> Right (Int.round n)
    Nothing -> Left (k <> " not numeric")

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
