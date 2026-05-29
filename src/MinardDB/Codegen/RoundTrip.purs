-- | Round-trip + drift smoke for the Schema ⇄ yoga-`Table` bridge.
-- |
-- |   1. parse Marginalia's real schema.sql → schemaA
-- |   2. emit yoga `Table` declarations → parse them back → schemaB
-- |   3. diff schemaA vs schemaB — expect EMPTY (the parser is a faithful
-- |      inverse of the generator, modulo the documented lossy fields,
-- |      which the diff is built to ignore).
-- |   4. drift demo: a deliberately-stale hand-written `Table` (a column
-- |      dropped, a type changed) is diffed against the real table — the
-- |      detector flags exactly those.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Codegen.RoundTrip
module MinardDB.Codegen.RoundTrip where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Codegen.YogaParse (parseYogaSchema)
import MinardDB.Codegen.YogaTable (emitModule)
import MinardDB.Migration.SQL (schemaFromSql)
import MinardDB.Schema (Schema)
import MinardDB.Schema.Diff (Diff, describeDiff, diffSchemas)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

schemaPath :: String
schemaPath =
  "/Users/afc/work/afc-work/agent-teams/project-tracker/database/schema.sql"

main :: Effect Unit
main = launchAff_ do
  schemaText <- FS.readTextFile UTF8 schemaPath
  case schemaFromSql "marginalia" schemaText of
    Left err -> liftEffect $ Console.log $ "SCHEMA PARSE FAILED — " <> err
    Right schemaA -> liftEffect do
      Console.log "── Bridge round-trip: Schema → yoga Table → Schema ──"
      let moduleText = emitModule "Generated.MarginaliaSchema" schemaA
      case parseYogaSchema "marginalia" moduleText of
        Left err -> Console.log $ "  REVERSE PARSE FAILED — " <> err
        Right schemaB -> do
          Console.log $ "  forward: " <> show (Array.length schemaA.tables) <> " tables"
          Console.log $ "  reverse: " <> show (Array.length schemaB.tables) <> " tables"
          report "round-trip drift (expect none)" (diffSchemas schemaA schemaB)
          Console.log ""
          driftDemo schemaA

-- | Show the detector catching real drift: hand-write a stale `projects`
-- | (drop `blog_content`, retype `status` to Int) and diff it against the
-- | real schema.
driftDemo :: Schema -> Effect Unit
driftDemo schemaA = do
  Console.log "── Drift demo: a stale hand-written binding vs the real schema ──"
  case parseYogaSchema "stale" staleModule of
    Left err -> Console.log $ "  parse failed — " <> err
    Right stale ->
      -- Compare just the projects tables (left = real, right = stale).
      case onlyProjects schemaA, onlyProjects stale of
        Just real, Just drifted ->
          report "drift vs stale binding (expect 2)" (diffSchemas real drifted)
        _, _ -> Console.log "  (projects not found in one side)"
  where
  onlyProjects s = (\t -> { name: s.name, tables: [ t ] })
    <$> Array.find (\t -> t.name == "projects") s.tables

report :: String -> Array Diff -> Effect Unit
report label diffs = do
  Console.log $ "  " <> label <> ": " <> show (Array.length diffs) <> " difference(s)"
  traverse_ (\d -> Console.log ("      • " <> describeDiff d)) diffs

-- A deliberately stale binding: `blog_content` removed, `status` retyped
-- from String to Int. Everything else matches the real projects table.
staleModule :: String
staleModule =
  """
  module Stale where
  import Yoga.Postgres.Schema (Table, PrimaryKey, AutoIncrement, Unique, Nullable, Default, DefaultExpr)
  import Data.DateTime (DateTime)

  type ProjectsTable = Table "projects"
    ( id :: PrimaryKey (AutoIncrement Int)
    , slug :: Unique (Nullable String)
    , parent_id :: Nullable Int
    , name :: String
    , domain :: String
    , subdomain :: Nullable String
    , status :: Default "idea" Int
    , evolved_into :: Nullable Int
    , description :: Nullable String
    , source_url :: Nullable String
    , source_path :: Nullable String
    , repo :: Nullable String
    , preferred_view :: Nullable String
    , cover_attachment_id :: Nullable Int
    , blog_status :: Nullable String
    , created_at :: DefaultExpr "current_timestamp" (Nullable DateTime)
    , updated_at :: DefaultExpr "current_timestamp" (Nullable DateTime)
    )
  """
