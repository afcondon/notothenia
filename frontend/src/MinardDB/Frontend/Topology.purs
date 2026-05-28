module MinardDB.Frontend.Topology
  ( SchemaData
  , SchemaTable
  , SchemaFK
  , Highlights
  , topologyView
  , parseSchemaData
  ) where

import Prelude

import Data.Argonaut.Core (Json, toArray, toNumber, toObject, toString)
import Data.Array (catMaybes, concatMap, elem, filter, find, fromFoldable, head, length, mapWithIndex, null, snoc, sortBy, tail, zip)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Int as Int
import Data.Maybe (Maybe(..), fromMaybe, isJust, maybe)
import Data.String as String
import Data.Traversable (traverse)
import Data.Tuple (Tuple(..), fst, snd)
import Foreign.Object (Object)
import Foreign.Object as Object
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Svg.Attributes as SA
import Halogen.Svg.Elements as SE

-- | A minimal schema view good enough for laying out the topology.
type SchemaData =
  { name :: String
  , tables :: Array SchemaTable
  , fks :: Array SchemaFK
  }

type SchemaTable =
  { name :: String
  , columnCount :: Int
  , primaryKey :: Array String
  }

type SchemaFK =
  { sourceTable :: String
  , sourceColumns :: Array String
  , refTable :: String
  , refColumns :: Array String
  , onDelete :: String
  }

-- | Tables to highlight in the rendering. The cycle witness gets a red
-- | border; the BCNF violator gets an orange fill. Both pulled from the
-- | proof receipt by the calling component.
type Highlights =
  { cycleWitness :: Maybe String   -- table name from the NoFKCycle witness atom
  , bcnfViolator :: Maybe String   -- table name from BCNF_<table>_... SAT row
  }

------------------------------------------------------------------------
-- Layout
------------------------------------------------------------------------

-- | Pixel sizes — tuned by eye on the three test schemas. Tables get
-- | wide enough to fit their longest name; the layered layout uses
-- | enough vertical gap that arrow heads don't collide with row labels.
tableHeight :: Number
tableHeight = 28.0

charWidth :: Number
charWidth = 7.5

tablePaddingX :: Number
tablePaddingX = 14.0

layerGap :: Number
layerGap = 80.0

intraGap :: Number
intraGap = 16.0

paddingX :: Number
paddingX = 24.0

paddingY :: Number
paddingY = 24.0

type LayoutNode =
  { name :: String
  , x :: Number          -- left edge
  , y :: Number          -- top edge
  , w :: Number
  , h :: Number
  , layer :: Int
  }

type LayoutEdge =
  { source :: String
  , target :: String
  , sourcePos :: { x :: Number, y :: Number }
  , targetPos :: { x :: Number, y :: Number }
  , isBack :: Boolean
  , isSelf :: Boolean
  }

type Layout =
  { nodes :: Array LayoutNode
  , edges :: Array LayoutEdge
  , width :: Number
  , height :: Number
  }

-- | Kahn's algorithm with a cycle-breaking fallback. Returns table names
-- | grouped by layer (sources first, leaves last). When no in-degree-0
-- | node is available among remaining names, we pop the alphabetically-
-- | first remaining name to break the deadlock and continue. Back-edges
-- | are detected during edge classification, not here.
computeLayers :: SchemaData -> Array (Array String)
computeLayers s =
  let
    initialIndeg :: Object Int
    initialIndeg =
      foldl
        (\acc fk -> Object.alter (Just <<< maybe 1 (_ + 1)) fk.refTable acc)
        (Object.fromFoldable (map (\t -> Tuple t.name 0) s.tables))
        (filter (\fk -> fk.sourceTable /= fk.refTable) s.fks)

    outgoing :: Object (Array String)
    outgoing = foldl
      (\acc fk ->
        if fk.sourceTable == fk.refTable then acc
        else Object.alter (Just <<< maybe [ fk.refTable ] (\xs -> snoc xs fk.refTable))
              fk.sourceTable acc)
      Object.empty
      s.fks

    allNames :: Array String
    allNames = map _.name s.tables

    loop :: Array String -> Object Int -> Array (Array String) -> Array (Array String)
    loop remaining indeg layers
      | null remaining = layers
      | otherwise =
          let
            sources = filter (\n -> Object.lookup n indeg == Just 0) remaining
            currentLayer =
              if null sources then
                -- Cycle break: peel the alphabetically first remaining
                -- table. Whatever incoming edges it has become "back-
                -- edges" once it goes into the current layer.
                fromMaybe [] (map (\h -> [ h ]) (head (sortBy compare remaining)))
              else
                sortBy compare sources
            newRemaining = filter (\n -> not (elem n currentLayer)) remaining
            newIndeg =
              foldl
                (\acc src ->
                  let targets = fromMaybe [] (Object.lookup src outgoing)
                  in foldl (\m t -> Object.alter (map (\v -> v - 1)) t m) acc targets)
                indeg
                currentLayer
          in
            loop newRemaining newIndeg (snoc layers currentLayer)
  in
    loop allNames initialIndeg []

-- | Walk the layered table list and assign coordinates.
assignCoords :: Array (Array String) -> Object SchemaTable -> { nodes :: Array LayoutNode, width :: Number, height :: Number }
assignCoords layers tableMap =
  let
    measure :: String -> Number
    measure n =
      max 70.0 (Int.toNumber (String.length n) * charWidth + 2.0 * tablePaddingX)

    layerWithWidths :: Array { tables :: Array { name :: String, w :: Number }, totalW :: Number }
    layerWithWidths = layers # map \names ->
      let
        sized = names # map \n -> { name: n, w: measure n }
        totalW =
          foldl (+) 0.0 (map _.w sized)
            + intraGap * Int.toNumber (max 0 (length sized - 1))
      in
        { tables: sized, totalW }

    maxLayerWidth = foldl max 0.0 (map _.totalW layerWithWidths)

    finalWidth = max 600.0 (maxLayerWidth + 2.0 * paddingX)

    centerX = finalWidth / 2.0

    nodes = layerWithWidths # mapWithIndex \layerIx layer ->
      let
        startX = centerX - layer.totalW / 2.0
        layerY = paddingY + Int.toNumber layerIx * (tableHeight + layerGap)
      in
        layer.tables # mapWithIndex \i t ->
          let
            offset = foldl (+) 0.0 (map _.w (Array.take i layer.tables))
                       + intraGap * Int.toNumber i
          in
            { name: t.name
            , x: startX + offset
            , y: layerY
            , w: t.w
            , h: tableHeight
            , layer: layerIx
            }

    flatNodes = Array.concat nodes
    totalH =
      Int.toNumber (length layers) * (tableHeight + layerGap)
        - layerGap
        + 2.0 * paddingY
    _ = tableMap  -- not currently used; reserved for column-count badge
  in
    { nodes: flatNodes, width: finalWidth, height: totalH }

classifyEdges :: Array LayoutNode -> Array SchemaFK -> Array LayoutEdge
classifyEdges nodes fks =
  let
    byName :: Object LayoutNode
    byName = Object.fromFoldable (map (\n -> Tuple n.name n) nodes)
  in
    fks # Array.mapMaybe \fk -> do
      src <- Object.lookup fk.sourceTable byName
      tgt <- Object.lookup fk.refTable byName
      let
        isSelf = fk.sourceTable == fk.refTable
        isBack = (not isSelf) && tgt.layer <= src.layer
        sourcePos = { x: src.x + src.w / 2.0, y: src.y + src.h }
        targetPos = { x: tgt.x + tgt.w / 2.0, y: tgt.y }
      pure
        { source: fk.sourceTable
        , target: fk.refTable
        , sourcePos
        , targetPos
        , isBack
        , isSelf
        }

computeLayout :: SchemaData -> Layout
computeLayout s =
  let
    layers = computeLayers s
    tableMap = Object.fromFoldable (map (\t -> Tuple t.name t) s.tables)
    laid = assignCoords layers tableMap
    edges = classifyEdges laid.nodes s.fks
  in
    { nodes: laid.nodes
    , edges
    , width: laid.width
    , height: laid.height
    }

------------------------------------------------------------------------
-- Render
------------------------------------------------------------------------

topologyView :: forall a action. SchemaData -> Highlights -> HH.HTML a action
topologyView schema hl =
  let
    layout = computeLayout schema
  in
    HH.section [ HP.class_ (HH.ClassName "topology-section") ]
      [ HH.h3_ [ HH.text "Topology" ]
      , topologyLegend
      , SE.svg
          [ SA.viewBox 0.0 0.0 layout.width layout.height
          , SA.class_ (HH.ClassName "topology-svg")
          ]
          ( map (renderEdge) layout.edges
              <> map (renderNode hl) layout.nodes
          )
      ]

topologyLegend :: forall a action. HH.HTML a action
topologyLegend =
  HH.div [ HP.class_ (HH.ClassName "topology-legend") ]
    [ legendSwatch "table-default" "table"
    , legendSwatch "table-bcnf" "BCNF violator"
    , legendSwatch "table-cycle" "cycle witness"
    , legendSwatch "edge-fwd" "forward FK"
    , legendSwatch "edge-back" "back-edge (cycle)"
    , legendSwatch "edge-self" "self-reference"
    ]

legendSwatch :: forall a action. String -> String -> HH.HTML a action
legendSwatch cls label =
  HH.span [ HP.class_ (HH.ClassName "legend-item") ]
    [ HH.span [ HP.class_ (HH.ClassName ("legend-swatch " <> cls)) ] []
    , HH.text label
    ]

renderNode :: forall a action. Highlights -> LayoutNode -> HH.HTML a action
renderNode hl n =
  let
    isBCNF = hl.bcnfViolator == Just n.name
    isCycle = hl.cycleWitness == Just n.name
    cls = String.joinWith " "
      $ [ "topo-table" ]
      <> (if isBCNF then [ "table-bcnf" ] else [])
      <> (if isCycle then [ "table-cycle" ] else [])
  in
    SE.g [ SA.class_ (HH.ClassName cls) ]
      [ SE.rect
          [ SA.x n.x
          , SA.y n.y
          , SA.width n.w
          , SA.height n.h
          , SA.rx 4.0
          ]
      , SE.text
          [ SA.x (n.x + n.w / 2.0)
          , SA.y (n.y + n.h / 2.0 + 4.0)
          , SA.textAnchor SA.AnchorMiddle
          ]
          [ HH.text n.name ]
      ]

renderEdge :: forall a action. LayoutEdge -> HH.HTML a action
renderEdge e
  | e.isSelf =
      -- Render a small loop above the source node.
      let
        sx = e.sourcePos.x
        sy = e.sourcePos.y - 28.0  -- approx top of node
      in
        SE.path
          [ SA.class_ (HH.ClassName "edge edge-self")
          , SA.d
              [ SA.m SA.Abs (sx - 6.0) sy
              , SA.c SA.Abs (sx - 24.0) (sy - 22.0) (sx + 24.0) (sy - 22.0) (sx + 6.0) sy
              ]
          , SA.fill SA.NoColor
          ]
  | e.isBack =
      -- Curve outward to one side; mark as cycle-back edge.
      let
        sx = e.sourcePos.x
        sy = e.sourcePos.y
        tx = e.targetPos.x
        ty = e.targetPos.y
        -- Bow out to the right of the source's column.
        ctrl1x = sx + 80.0
        ctrl1y = (sy + ty) / 2.0
        ctrl2x = tx + 80.0
        ctrl2y = ctrl1y
      in
        SE.path
          [ SA.class_ (HH.ClassName "edge edge-back")
          , SA.d
              [ SA.m SA.Abs sx sy
              , SA.c SA.Abs ctrl1x ctrl1y ctrl2x ctrl2y tx ty
              ]
          , SA.fill SA.NoColor
          ]
  | otherwise =
      -- Forward edge — slight curve so parallel edges don't superimpose.
      let
        sx = e.sourcePos.x
        sy = e.sourcePos.y
        tx = e.targetPos.x
        ty = e.targetPos.y
        midY = (sy + ty) / 2.0
      in
        SE.path
          [ SA.class_ (HH.ClassName "edge edge-fwd")
          , SA.d
              [ SA.m SA.Abs sx sy
              , SA.c SA.Abs sx midY tx midY tx ty
              ]
          , SA.fill SA.NoColor
          ]

------------------------------------------------------------------------
-- JSON parser for the schema sub-object served by the backend
------------------------------------------------------------------------

parseSchemaData :: Json -> Either String SchemaData
parseSchemaData j = do
  obj <- toObject j # note "schema not an object"
  name <- objStr obj "name"
  tablesJ <- objArr obj "tables"
  tables <- traverse parseTable tablesJ
  fksJ <- objArr obj "fks"
  fks <- traverse parseFK fksJ
  pure { name, tables, fks }

parseTable :: Json -> Either String SchemaTable
parseTable j = do
  obj <- toObject j # note "table not an object"
  name <- objStr obj "name"
  columnCount <- objInt obj "columnCount"
  pkJ <- objArr obj "primaryKey"
  primaryKey <- traverse (\v -> toString v # note "PK col not a string") pkJ
  pure { name, columnCount, primaryKey }

parseFK :: Json -> Either String SchemaFK
parseFK j = do
  obj <- toObject j # note "fk not an object"
  sourceTable <- objStr obj "sourceTable"
  srcColsJ <- objArr obj "sourceColumns"
  sourceColumns <- traverse (\v -> toString v # note "src col not a string") srcColsJ
  refTable <- objStr obj "refTable"
  refColsJ <- objArr obj "refColumns"
  refColumns <- traverse (\v -> toString v # note "ref col not a string") refColsJ
  onDelete <- objStr obj "onDelete"
  pure { sourceTable, sourceColumns, refTable, refColumns, onDelete }

objStr :: Object Json -> String -> Either String String
objStr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> toString v # note (k <> " not a string"))

objArr :: Object Json -> String -> Either String (Array Json)
objArr o k = Object.lookup k o # note ("missing " <> k)
  >>= (\v -> toArray v # note (k <> " not an array"))

objInt :: Object Json -> String -> Either String Int
objInt o k = Object.lookup k o # note ("missing " <> k)
  >>= \v -> case toNumber v of
    Just n -> Right (Int.round n)
    Nothing -> Left (k <> " not numeric")

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg
