-- | Smoke test for query reach analysis (Phase 4).
-- |
-- | A small blog-shaped schema with one deliberately-dead column
-- | (`users.legacy_token` — no query touches it) and a handful of
-- | queries that exercise the resolver: qualified refs, bare refs,
-- | aliases, `*`, `t.*`, a JOIN, and an UPDATE. The expected outcome:
-- | every column except `legacy_token` is reached, and it alone shows
-- | up as dead.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Query.Smoke
module MinardDB.Query.Smoke where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import MinardDB.Query (parseQuery)
import MinardDB.Query.Reach (deadColumns, resolveReach, showColumnId)
import MinardDB.Query.Report (LabeledQuery, blogQueries, blogSchema)
import MinardDB.Schema (Schema)

-- Reuse the canonical fixture (single source of truth lives in
-- MinardDB.Query.Report, which also drives the frontend reach view).
schema :: Schema
schema = blogSchema

queries :: Array LabeledQuery
queries = blogQueries

------------------------------------------------------------------------
-- Runner
------------------------------------------------------------------------

main :: Effect Unit
main = do
  Console.log "── Query reach analysis smoke ──"
  Console.log ""
  traverse_ runOne queries
  -- Aggregate dead-column pass over all queries that parsed.
  let refs = Array.mapMaybe (\q -> hush (parseQuery q.sql)) queries
  Console.log ""
  Console.log "── Aggregate dead-column report ──"
  let report = deadColumns schema refs
  Console.log $ "  " <> show (Array.length report.reached) <> "/"
    <> show report.totalColumns <> " columns reached by "
    <> show (Array.length refs) <> " queries"
  case report.dead of
    [] -> Console.log "  ✓ no dead columns"
    dead -> do
      Console.log $ "  ⚠ " <> show (Array.length dead) <> " dead column(s):"
      traverse_ (\c -> Console.log ("      " <> showColumnId c)) dead

runOne :: { label :: String, sql :: String } -> Effect Unit
runOne q = case parseQuery q.sql of
  Left err -> Console.log $ "[" <> q.label <> "] PARSE ERROR — " <> err
  Right refs -> do
    let reach = resolveReach schema refs
    Console.log $ "[" <> q.label <> "] " <> show refs.kind
    Console.log $ "    tables: " <> joinOrDash reach.tables
    Console.log $ "    touched: " <> joinOrDash (map showColumnId reach.touched)
    case reach.ambiguous of
      [] -> pure unit
      a -> Console.log $ "    ambiguous bare cols: " <> joinOrDash a
    case reach.unresolved of
      [] -> pure unit
      u -> Console.log $ "    unresolved: " <> joinOrDash u

joinOrDash :: Array String -> String
joinOrDash xs = case xs of
  [] -> "—"
  _ -> Array.intercalate ", " xs

hush :: forall a b. Either a b -> Maybe b
hush = case _ of
  Right b -> Just b
  Left _ -> Nothing
