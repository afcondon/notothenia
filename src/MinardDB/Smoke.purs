module MinardDB.Smoke where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Class.Console as Console
import MinardDB.Alloy.Generate (generate)
import MinardDB.Schema (FKAction(..), PGType(..), Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Sync as FS

-- | A small hand-written schema modeling a slice of Minard's own DB:
-- |   projects(id PK, name, repo_url)
-- |   snapshots(id PK, project_id FK->projects, git_hash)
-- |   packages(id PK, snapshot_id FK->snapshots, name)
-- |
-- | This is the test fixture for the end-to-end Alloy loop.
smokeSchema :: Schema
smokeSchema =
  { name: "minard-smoke"
  , tables:
      [ { name: "projects"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "name", dataType: PGText, nullable: false, defaultExpr: Nothing }
            , { name: "repo_url", dataType: PGText, nullable: true, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys: []
        , uniqueConstraints: [ { columns: [ "name" ] } ]
        }
      , { name: "snapshots"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "project_id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "git_hash", dataType: PGText, nullable: false, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys:
            [ { columns: [ "project_id" ]
              , refTable: "projects"
              , refColumns: [ "id" ]
              , onDelete: Cascade
              , onUpdate: NoAction
              }
            ]
        , uniqueConstraints: []
        }
      , { name: "packages"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "snapshot_id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "name", dataType: PGText, nullable: false, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys:
            [ { columns: [ "snapshot_id" ]
              , refTable: "snapshots"
              , refColumns: [ "id" ]
              , onDelete: Cascade
              , onUpdate: NoAction
              }
            ]
        , uniqueConstraints: []
        }
      ]
  }

main :: Effect Unit
main = do
  let alsText = generate smokeSchema
  Console.log "=== Generated Alloy model ==="
  Console.log alsText
  Console.log ""
  Console.log "=== Writing to /tmp/minard-smoke.als ==="
  FS.writeTextFile UTF8 "/tmp/minard-smoke.als" alsText
  Console.log "Done. Run with:"
  Console.log "  /opt/homebrew/opt/openjdk/bin/java -jar vendor/alloy.jar exec /tmp/minard-smoke.als"
