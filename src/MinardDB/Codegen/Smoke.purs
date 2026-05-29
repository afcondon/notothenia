-- | Forward bridge cut: generate rowtype-yoga `Table` type declarations
-- | from Marginalia's real schema, focusing on `projects`.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Codegen.Smoke
module MinardDB.Codegen.Smoke where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Codegen.YogaTable (emitModule, emitTable)
import MinardDB.Migration.SQL (schemaFromSql)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

schemaPath :: String
schemaPath =
  "/Users/afc/work/afc-work/agent-teams/project-tracker/database/schema.sql"

outDir :: String
outDir = "generated"

main :: Effect Unit
main = launchAff_ do
  schemaText <- FS.readTextFile UTF8 schemaPath
  case schemaFromSql "marginalia" schemaText of
    Left err -> liftEffect $ Console.log $ "SCHEMA PARSE FAILED — " <> err
    Right schema -> do
      liftEffect do
        Console.log "── Forward bridge: notothenia Schema → yoga Table ──"
        Console.log ""
        case Array.find (\t -> t.name == "projects") schema.tables of
          Just projects -> do
            Console.log "Generated declaration for `projects`:"
            Console.log ""
            Console.log (emitTable projects)
          Nothing -> Console.log "no `projects` table found"

      -- Write the whole schema as a compilable module, for compile-
      -- validation against yoga in a consuming project.
      _ <- attempt (FS.mkdir outDir)
      let modText = emitModule "Generated.MarginaliaSchema" schema
      let outPath = Path.concat [ outDir, "MarginaliaSchema.purs" ]
      FS.writeTextFile UTF8 outPath modText
      liftEffect $ Console.log $ "\nWrote full module → " <> outPath
        <> " (" <> show (Array.length schema.tables) <> " tables)"
