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
import Data.String as String
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
  , minScope :: Maybe Int
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
    , noDeclaredFKsExplainer d
    , inferredFKsSection d.inferredFKs
    , cycleExplainer d
    , bcnfExplainer d
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
      ]
  else
    HH.text ""

noDeclaredFKsExplainer :: forall a. Detail -> HH.HTML a Action
noDeclaredFKsExplainer d =
  if d.summary.declaredFKCount == 0 && d.summary.inferredFKCount > 0 then
    HH.section [ HP.class_ (HH.ClassName "explainer") ]
      [ HH.h3_ [ HH.text "What is a foreign key, and why does this matter?" ]
      , HH.p_
          [ HH.text "A "
          , HH.em_ [ HH.text "foreign key" ]
          , HH.text " is a constraint that says: "
          , HH.q_ [ HH.text "this column must point at a real row in some other table." ]
          ]
      , HH.p_
          [ HH.text "Concretely: when an application creates a note about a project, that note has a "
          , HH.code_ [ HH.text "project_id" ]
          , HH.text " column. A foreign key constraint would tell the database: "
          , HH.q_ [ HH.text "this project_id must match an actual id in the projects table." ]
          ]
      , HH.p_ [ HH.text "When the database enforces foreign keys:" ]
      , HH.ul_
          [ HH.li_ [ HH.text "It refuses to insert a note pointing at a project that doesn't exist." ]
          , HH.li_ [ HH.text "It can automatically delete the notes when you delete the project (ON DELETE CASCADE)." ]
          , HH.li_ [ HH.text "It guarantees that JOINs return real data, not orphans." ]
          ]
      , HH.h4_ [ HH.text "What it means that none are declared" ]
      , HH.p_
          [ HH.text "Your schema has "
          , HH.strong_ [ HH.text (show d.summary.inferredFKCount) ]
          , HH.text " columns that "
          , HH.em_ [ HH.text "look like" ]
          , HH.text " foreign keys (named "
          , HH.code_ [ HH.text "project_id" ]
          , HH.text ", "
          , HH.code_ [ HH.text "tag_id" ]
          , HH.text ", etc.) but none are declared as constraints. Right now, nothing in the database itself prevents:"
          ]
      , HH.ul_
          [ HH.li_
              [ HH.text "A "
              , HH.code_ [ HH.text "project_notes" ]
              , HH.text " row with "
              , HH.code_ [ HH.text "project_id = 99999" ]
              , HH.text " (where no such project exists)."
              ]
          , HH.li_
              [ HH.text "Half-deleted state: removing a project leaves its notes, servers, tags, and dependencies as orphans pointing into the void."
              ]
          , HH.li_
              [ HH.text "Bugs and ad-hoc SQL silently producing inconsistent data."
              ]
          ]
      , HH.p_
          [ HH.text "All of this is enforced (if at all) by application code. Every place that inserts or updates a row has to remember to validate references; every place that deletes has to remember to clean up children. Miss one path and you get corruption."
          ]
      , HH.h4_ [ HH.text "How to fix" ]
      , HH.p_
          [ HH.text "For each candidate FK below, add a declaration like:" ]
      , HH.pre_
          [ HH.code_ [ HH.text
              "ALTER TABLE project_notes\n  ADD FOREIGN KEY (project_id)\n  REFERENCES projects(id)\n  ON DELETE CASCADE;"
            ]
          ]
      , HH.p_
          [ HH.text "Before declaring, you need to (a) find and clean up any existing orphan rows, and (b) decide the appropriate "
          , HH.code_ [ HH.text "ON DELETE" ]
          , HH.text " action: "
          , HH.code_ [ HH.text "CASCADE" ]
          , HH.text " (delete children too), "
          , HH.code_ [ HH.text "RESTRICT" ]
          , HH.text " (refuse the delete if children exist), or "
          , HH.code_ [ HH.text "SET NULL" ]
          , HH.text " (orphan the children but keep them — only when the column is nullable)."
          ]
      , HH.p_
          [ HH.text "The "
          , HH.strong_ [ HH.text (show d.summary.inferredFKCount) ]
          , HH.text " candidates in the table below are our best guess based on column naming. Some may be wrong — review each before adopting."
          ]
      ]
  else
    HH.text ""

cycleExplainer :: forall a. Detail -> HH.HTML a Action
cycleExplainer d =
  case findCycleProof d of
    Nothing -> HH.text ""
    Just witnessTable ->
      HH.section [ HP.class_ (HH.ClassName "explainer") ]
        [ HH.h3_ [ HH.text "Why is there a cycle, and what does that mean?" ]
        , HH.p_
            [ HH.text "The proof "
            , HH.code_ [ HH.text "NoFKCycle" ]
            , HH.text " found a counterexample in "
            , HH.code_ [ HH.text witnessTable ]
            , HH.text ". This is almost always because that table has a "
            , HH.em_ [ HH.text "self-referencing" ]
            , HH.text " foreign key — a column on the table that points back to the same table."
            ]
        , HH.p_
            [ HH.text "Self-references represent trees. A project can have a parent project; a namespace can have a parent namespace. Conceptually it's a tree — but the database doesn't know it's supposed to be a tree. It only knows: "
            , HH.q_ [ HH.text "this column points at some row in the same table." ]
            , HH.text " Nothing prevents pointing at yourself, or in a circle."
            ]
        , HH.p_
            [ HH.text "A "
            , HH.em_ [ HH.text "cycle" ]
            , HH.text " would mean: A's parent is B, and B's parent is A. Or longer: A → B → C → A. In a real tree this is logical nonsense — neither node is "
            , HH.q_ [ HH.text "above" ]
            , HH.text " the other. But the schema permits it, and the proof solver found a specific configuration where it happens: a row that ends up as its own ancestor."
            ]
        , HH.h4_ [ HH.text "Why this matters" ]
        , HH.ul_
            [ HH.li_
                [ HH.text "Code that walks the parent chain — "
                , HH.q_ [ HH.text "show me this project and all its ancestors" ]
                , HH.text " — loops forever if it hits a cycle. Stack overflow or a hung request."
                ]
            , HH.li_
                [ HH.text "Aggregations — "
                , HH.q_ [ HH.text "roll up time spent across all subprojects" ]
                , HH.text " — either loop or double-count, depending on the query."
                ]
            , HH.li_
                [ HH.text "A new developer writing tree-walking code has no way to know they need to defend against cycles. The schema gives no warning."
                ]
            ]
        , HH.h4_ [ HH.text "How to fix" ]
        , HH.p_
            [ HH.text "Database-level options, ordered by completeness:" ]
        , HH.ol_
            [ HH.li_
                [ HH.strong_ [ HH.text "Application-side check on every UPDATE." ]
                , HH.text " Before changing a parent_id, walk up the chain to verify no cycle would result. Simple, but easy to forget — a single bug bypasses it."
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "CHECK constraint." ]
                , HH.text " "
                , HH.code_ [ HH.text "CHECK (id != parent_id)" ]
                , HH.text " prevents direct self-loops, but not multi-step cycles. Better than nothing; DuckDB supports it."
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "BEFORE-UPDATE trigger." ]
                , HH.text " A trigger that walks the parent chain at insert/update time and rejects the change if it would create a cycle. Complete but DB-specific."
                ]
            , HH.li_
                [ HH.strong_ [ HH.text "Closure table." ]
                , HH.text " A separate table tracking every (ancestor, descendant) pair, kept in sync via triggers. Cycles become physically impossible because they would require inserting (A, A), which violates a uniqueness constraint. Most robust but requires changes to all tree-walking code."
                ]
            ]
        ]

-- | Detect a NoFKCycle counterexample and pull out the witness's table name.
findCycleProof :: Detail -> Maybe String
findCycleProof d =
  Array.find (\p -> p.commandName == "NoFKCycle" && p.verdict == "SAT") d.proofs
    >>= _.witness
    >>= extractWitnessTable

-- | A witness atom like `module_namespaces$7` → table name `module_namespaces`.
extractWitnessTable :: String -> Maybe String
extractWitnessTable atom =
  case Array.head (String.split (String.Pattern "$") atom) of
    Just t | t /= "" -> Just t
    _ -> Nothing

--------------------------------------------------------------------------
-- BCNF explainer
--------------------------------------------------------------------------

type BCNFFinding =
  { checkName :: String     -- e.g. "BCNF_addresses_zip__city"
  , witnessTable :: String  -- e.g. "addresses"
  , determinant :: String   -- e.g. "zip"  (joined with ", " if multi-col)
  , dependent :: String     -- e.g. "city" (joined with ", " if multi-col)
  }

bcnfExplainer :: forall a. Detail -> HH.HTML a Action
bcnfExplainer d = case findBCNFProof d of
  Nothing -> HH.text ""
  Just f ->
    HH.section [ HP.class_ (HH.ClassName "explainer") ]
      [ HH.h3_
          [ HH.text "Why does the same address have two cities? (BCNF)" ]
      , HH.p_
          [ HH.text "The proof "
          , HH.code_ [ HH.text f.checkName ]
          , HH.text " found two distinct rows of "
          , HH.code_ [ HH.text f.witnessTable ]
          , HH.text " that agree on "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text " but disagree on "
          , HH.code_ [ HH.text f.dependent ]
          , HH.text ". The schema permits this because "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text " isn't a key — there's no PK or UNIQUE constraint forcing values to be distinct."
          ]
      , HH.p_
          [ HH.text "But the table claims "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text " "
          , HH.em_ [ HH.text "determines" ]
          , HH.text " "
          , HH.code_ [ HH.text f.dependent ]
          , HH.text " — i.e. that knowing the first tells you the second. If the schema permits two different "
          , HH.code_ [ HH.text f.dependent ]
          , HH.text " values for the same "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text ", that claim isn't enforceable. This is a "
          , HH.strong_ [ HH.text "BCNF violation" ]
          , HH.text " (Boyce–Codd Normal Form): every non-trivial functional dependency must have a "
          , HH.em_ [ HH.text "superkey" ]
          , HH.text " as its determinant, otherwise the schema admits inconsistent data."
          ]
      , HH.h4_ [ HH.text "What this looks like in real data" ]
      , HH.p_
          [ HH.text "Two rows that "
          , HH.em_ [ HH.text "should" ]
          , HH.text " agree but don't:" ]
      , HH.pre_
          [ HH.code_
              [ HH.text ("INSERT INTO " <> f.witnessTable <> " VALUES (1, '02139', 'Cambridge', '...');\n")
              , HH.text ("INSERT INTO " <> f.witnessTable <> " VALUES (2, '02139', 'Somerville', '...');\n")
              , HH.text "-- both rows valid by the schema; both rows wrong in reality"
              ]
          ]
      , HH.h4_ [ HH.text "Why this matters" ]
      , HH.ul_
          [ HH.li_
              [ HH.strong_ [ HH.text "Redundancy." ]
              , HH.text " Every row repeats the "
              , HH.code_ [ HH.text f.dependent ]
              , HH.text " value. Storage is fine; the problem is "
              , HH.em_ [ HH.text "update anomalies" ]
              , HH.text "."
              ]
          , HH.li_
              [ HH.strong_ [ HH.text "Update anomaly." ]
              , HH.text " When the "
              , HH.code_ [ HH.text f.dependent ]
              , HH.text " for a given "
              , HH.code_ [ HH.text f.determinant ]
              , HH.text " changes, you have to update every row. Miss one and the database now disagrees with itself."
              ]
          , HH.li_
              [ HH.strong_ [ HH.text "Insertion anomaly." ]
              , HH.text " You can't record a "
              , HH.code_ [ HH.text f.determinant ]
              , HH.text "/"
              , HH.code_ [ HH.text f.dependent ]
              , HH.text " pair until a row of "
              , HH.code_ [ HH.text f.witnessTable ]
              , HH.text " needs it — the fact has nowhere else to live."
              ]
          , HH.li_
              [ HH.strong_ [ HH.text "Deletion anomaly." ]
              , HH.text " Deleting the last row referencing a particular "
              , HH.code_ [ HH.text f.determinant ]
              , HH.text " forgets the "
              , HH.code_ [ HH.text f.dependent ]
              , HH.text " entirely."
              ]
          ]
      , HH.h4_ [ HH.text "How to fix" ]
      , HH.p_
          [ HH.text "The textbook fix: split the table so the "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text " → "
          , HH.code_ [ HH.text f.dependent ]
          , HH.text " relationship lives in its own table where "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text " is the primary key." ]
      , HH.pre_
          [ HH.code_
              [ HH.text ("-- Move the fact out:\n")
              , HH.text ("CREATE TABLE " <> f.determinant <> "_lookup (\n")
              , HH.text ("  " <> f.determinant <> " TEXT PRIMARY KEY,\n")
              , HH.text ("  " <> f.dependent <> " TEXT NOT NULL\n")
              , HH.text (");\n\n")
              , HH.text ("-- " <> f.witnessTable <> " keeps the FK, drops the redundant column:\n")
              , HH.text ("ALTER TABLE " <> f.witnessTable <> " DROP COLUMN " <> f.dependent <> ";\n")
              , HH.text ("ALTER TABLE " <> f.witnessTable <> " ADD FOREIGN KEY (" <> f.determinant <> ")\n")
              , HH.text ("  REFERENCES " <> f.determinant <> "_lookup(" <> f.determinant <> ");")
              ]
          ]
      , HH.p_
          [ HH.text "Now there is exactly one row per "
          , HH.code_ [ HH.text f.determinant ]
          , HH.text ", the relationship is enforceable by the PK on the lookup table, and BCNF holds."
          ]
      ]

-- | Detect a BCNF SAT counterexample, extract the offending FD from the
-- | check name (format: `BCNF_<table>_<det_cols>__<dep_cols>`), and pull
-- | the witness table from the witness atom. The double-underscore
-- | separates the determinant from the dependent.
findBCNFProof :: Detail -> Maybe BCNFFinding
findBCNFProof d = do
  proof <- Array.find isBCNFViolation d.proofs
  witness <- proof.witness
  table <- extractWitnessTable witness
  { determinant, dependent } <- parseBCNFCheckName proof.commandName
  pure
    { checkName: proof.commandName
    , witnessTable: table
    , determinant
    , dependent
    }
  where
    isBCNFViolation p =
      case String.stripPrefix (String.Pattern "BCNF_") p.commandName of
        Just _ -> p.verdict == "SAT"
        Nothing -> false

-- | Parse a check name like `BCNF_addresses_zip__city` into determinant
-- | and dependent column lists. The `__` (double-underscore) splits the
-- | two; the table-name boundary inside the first half is recovered
-- | by best-effort heuristic (split off the first underscore-separated
-- | segment as the table).
parseBCNFCheckName
  :: String -> Maybe { determinant :: String, dependent :: String }
parseBCNFCheckName name = do
  rest <- String.stripPrefix (String.Pattern "BCNF_") name
  let parts = String.split (String.Pattern "__") rest
  case parts of
    [ tableAndDet, dep ] -> do
      -- table_det1_det2_...  →  drop the first segment (table), join rest with ", "
      let segs = String.split (String.Pattern "_") tableAndDet
      detSegs <- Array.tail segs
      Just
        { determinant: String.joinWith ", " detSegs
        , dependent: String.joinWith ", " (String.split (String.Pattern "_") dep)
        }
    _ -> Nothing

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
    , scopeFootnote proofs
    ]

proofRow :: forall a. Proof -> HH.HTML a Action
proofRow p =
  HH.tr [ HP.class_ (HH.ClassName ("proof-row " <> verdictClass p)) ]
    [ HH.td_ [ HH.code_ [ HH.text p.commandName ] ]
    , HH.td_ [ HH.text p.kind ]
    , HH.td_ [ scopeCell p ]
    , HH.td [ HP.class_ (HH.ClassName "verdict") ]
        [ HH.text p.verdict ]
    , HH.td_ [ HH.text p.interpretation ]
    , HH.td_ [ case p.witness of
        Just w -> HH.code_ [ HH.text w ]
        Nothing -> HH.text "—"
      ]
    ]

-- | Scope cell: shows "N" normally, or "N → m" when minimization found a
-- | smaller scope at which the property still breaks (m < N). The arrow
-- | mirrors QuickCheck's "shrunken counterexample" notation.
scopeCell :: forall a. Proof -> HH.HTML a Action
scopeCell p = case p.minScope of
  Just m | m < p.scope ->
    HH.span_
      [ HH.text (show p.scope)
      , HH.span [ HP.class_ (HH.ClassName "min-scope") ]
          [ HH.text (" → " <> show m) ]
      ]
  _ -> HH.text (show p.scope)

-- | Footnote explaining scope notation if any row has been minimized.
scopeFootnote :: forall a. Array Proof -> HH.HTML a Action
scopeFootnote proofs =
  if Array.any wasMinimized proofs then
    HH.p [ HP.class_ (HH.ClassName "scope-footnote") ]
      [ HH.text "Scope is the bound on rows-per-table Alloy considers. "
      , HH.code_ [ HH.text "N → m" ]
      , HH.text " means the property first failed at scope "
      , HH.code_ [ HH.text "N" ]
      , HH.text " and still fails at scope "
      , HH.code_ [ HH.text "m" ]
      , HH.text " (binary-searched downward, like a property-test shrinker). Smaller minimum scopes mean more debuggable counterexamples — "
      , HH.code_ [ HH.text "m = 1" ]
      , HH.text " is a single-row witness."
      ]
  else
    HH.text ""
  where
    wasMinimized p = case p.minScope of
      Just m -> m < p.scope
      Nothing -> false

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
  let minScope = case Object.lookup "minScope" obj of
        Just mj | not (J.isNull mj) -> case toNumber mj of
          Just n -> Just (Int.round n)
          Nothing -> Nothing
        _ -> Nothing
  pure { commandName, kind, source, verdict, interpretation, witness, scope, minScope }

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
