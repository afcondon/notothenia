-- | Generate query-reach reports as JSON, for the frontend reach view
-- | (Phase 4b).
-- |
-- | Same precompute → store → serve → render shape as
-- | `MinardDB.Migration.Report`: given a schema and a set of queries,
-- | resolve each query's reach, count how many queries touch each
-- | column, flag the dead ones, and write a compact JSON report to
-- | `reports/reach-<name>.json`. The backend serves it; the frontend
-- | renders the schema as a usage heatmap (every column tinted by its
-- | reach count, dead columns flagged) plus a per-query footprint list.
-- |
-- | The fixture schema + queries live here (single source of truth);
-- | `MinardDB.Query.Smoke` imports them.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Query.Report
module MinardDB.Query.Report
  ( ReachReport
  , TableUsage
  , ColumnUsage
  , QueryFootprint
  , LabeledQuery
  , blogSchema
  , blogQueries
  , buildReachReport
  , encodeReachReport
  , main
  ) where

import Prelude

import Data.Argonaut.Core (Json, stringify)
import Data.Argonaut.Core as J
import Data.Array as Array
import Data.Either (Either(..))
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.Common (joinWith, split, trim) as Str
import Data.Tuple.Nested ((/\))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import Foreign.Object as Object
import MinardDB.Query (parseQuery)
import MinardDB.Query.Reach (ColumnId, Reach, resolveReach)
import MinardDB.Schema (Column, PGType(..), Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

------------------------------------------------------------------------
-- Report model
------------------------------------------------------------------------

type LabeledQuery = { label :: String, sql :: String }

-- | A per-query footprint: what the query is and what it reaches.
type QueryFootprint =
  { label :: String
  , kind :: String
  , sql :: String
  , tables :: Array String
  , touched :: Array String      -- "table.column" strings
  , ambiguous :: Array String
  , unresolved :: Array String
  , parseError :: Maybe String
  }

type ColumnUsage =
  { name :: String
  , reachCount :: Int            -- # of queries touching this column
  , dead :: Boolean
  }

type TableUsage =
  { name :: String
  , columns :: Array ColumnUsage
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
-- Build
------------------------------------------------------------------------

buildReachReport :: String -> String -> Schema -> Array LabeledQuery -> ReachReport
buildReachReport name blurb schema labeled =
  let
    -- Parse each query; keep both the footprint (for display, incl.
    -- parse failures) and the resolved reach (for counting).
    analyzed = map analyze labeled

    reaches :: Array Reach
    reaches = Array.mapMaybe _.reach analyzed

    -- Count, per schema column, how many queries' reach touches it.
    tables = schema.tables # map \t ->
      { name: t.name
      , columns: t.columns # map \c ->
          let n = countTouching reaches { table: t.name, column: c.name }
          in { name: c.name, reachCount: n, dead: n == 0 }
      }

    allColumns = Array.concatMap _.columns tables
    deadCount = Array.length (Array.filter _.dead allColumns)
  in
    { name
    , blurb
    , schemaName: schema.name
    , totalColumns: Array.length allColumns
    , reachedCount: Array.length allColumns - deadCount
    , deadCount
    , tables
    , queries: map _.footprint analyzed
    }
  where
  analyze q = case parseQuery q.sql of
    Left err ->
      { reach: Nothing
      , footprint:
          { label: q.label
          , kind: "?"
          , sql: trim q.sql
          , tables: []
          , touched: []
          , ambiguous: []
          , unresolved: []
          , parseError: Just err
          }
      }
    Right refs ->
      let reach = resolveReach schema refs
      in
        { reach: Just reach
        , footprint:
            { label: q.label
            , kind: show refs.kind
            , sql: trim q.sql
            , tables: reach.tables
            , touched: map showCol reach.touched
            , ambiguous: reach.ambiguous
            , unresolved: reach.unresolved
            , parseError: Nothing
            }
        }

  showCol c = c.table <> "." <> c.column

countTouching :: Array Reach -> ColumnId -> Int
countTouching reaches target =
  Array.length (Array.filter (\r -> Array.any (sameCol target) r.touched) reaches)
  where
  sameCol a b = a.table == b.table && a.column == b.column

-- Collapse the fixture's indented, multi-line SQL into a single tidy
-- line so the stored string displays cleanly in the UI.
trim :: String -> String
trim =
  Str.joinWith " "
    <<< Array.filter (_ /= "")
    <<< map Str.trim
    <<< Str.split (Pattern "\n")

------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------

encodeReachReport :: ReachReport -> Json
encodeReachReport r = J.fromObject $ Object.fromFoldable
  [ "name"         /\ J.fromString r.name
  , "blurb"        /\ J.fromString r.blurb
  , "schemaName"   /\ J.fromString r.schemaName
  , "totalColumns" /\ J.fromNumber (Int.toNumber r.totalColumns)
  , "reachedCount" /\ J.fromNumber (Int.toNumber r.reachedCount)
  , "deadCount"    /\ J.fromNumber (Int.toNumber r.deadCount)
  , "tables"       /\ J.fromArray (map encodeTable r.tables)
  , "queries"      /\ J.fromArray (map encodeQuery r.queries)
  ]

encodeTable :: TableUsage -> Json
encodeTable t = J.fromObject $ Object.fromFoldable
  [ "name"    /\ J.fromString t.name
  , "columns" /\ J.fromArray (map encodeColumn t.columns)
  ]

encodeColumn :: ColumnUsage -> Json
encodeColumn c = J.fromObject $ Object.fromFoldable
  [ "name"       /\ J.fromString c.name
  , "reachCount" /\ J.fromNumber (Int.toNumber c.reachCount)
  , "dead"       /\ J.fromBoolean c.dead
  ]

encodeQuery :: QueryFootprint -> Json
encodeQuery q = J.fromObject $ Object.fromFoldable
  [ "label"      /\ J.fromString q.label
  , "kind"       /\ J.fromString q.kind
  , "sql"        /\ J.fromString q.sql
  , "tables"     /\ J.fromArray (map J.fromString q.tables)
  , "touched"    /\ J.fromArray (map J.fromString q.touched)
  , "ambiguous"  /\ J.fromArray (map J.fromString q.ambiguous)
  , "unresolved" /\ J.fromArray (map J.fromString q.unresolved)
  , "parseError" /\ case q.parseError of
      Just e -> J.fromString e
      Nothing -> J.jsonNull
  ]

------------------------------------------------------------------------
-- Fixture: a blog schema with one deliberately-dead column
------------------------------------------------------------------------

col :: String -> PGType -> Column
col name dataType = { name, dataType, nullable: false, defaultExpr: Nothing }

blogSchema :: Schema
blogSchema =
  { name: "blog"
  , tables:
      [ { name: "users"
        , schemaName: "main"
        , columns: [ col "id" PGInt, col "name" PGText, col "email" PGText, col "legacy_token" PGText ]
        , primaryKey: [ "id" ]
        , foreignKeys: []
        , uniqueConstraints: []
        , functionalDependencies: []
        }
      , { name: "posts"
        , schemaName: "main"
        , columns:
            [ col "id" PGInt, col "author_id" PGInt, col "title" PGText
            , col "body" PGText, col "published" PGBoolean
            ]
        , primaryKey: [ "id" ]
        , foreignKeys: []
        , uniqueConstraints: []
        , functionalDependencies: []
        }
      ]
  }

blogQueries :: Array LabeledQuery
blogQueries =
  [ { label: "post list with author"
    , sql:
        """
        SELECT u.name, u.email, p.title, p.body
        FROM posts p
        JOIN users u ON p.author_id = u.id
        WHERE p.published = true
        ORDER BY p.id
        """
    }
  , { label: "published post ids"
    , sql: "SELECT id, title FROM posts WHERE published = true"
    }
  , { label: "full post row"
    , sql: "SELECT * FROM posts WHERE id = 1"
    }
  , { label: "publish a post"
    , sql: "UPDATE posts SET published = true WHERE id = 42"
    }
  ]

------------------------------------------------------------------------
-- main
------------------------------------------------------------------------

reportsDir :: String
reportsDir = "reports"

main :: Effect Unit
main = launchAff_ do
  _ <- attempt (FS.mkdir reportsDir)
  let report = buildReachReport "blog"
        "A blog schema + four queries; users.legacy_token is dead."
        blogSchema blogQueries
  let outPath = Path.concat [ reportsDir, "reach-blog.json" ]
  FS.writeTextFile UTF8 outPath (stringify (encodeReachReport report))
  liftEffect $ Console.log $ "Wrote " <> outPath
    <> " — " <> show report.reachedCount <> "/" <> show report.totalColumns
    <> " columns reached, " <> show report.deadCount <> " dead"
