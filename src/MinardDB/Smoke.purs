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

-- | A schema with a deliberate FK cycle: A -> B -> A.
-- | Alloy should report the NoFKCycle check as SAT (counterexample found).
cyclicSchema :: Schema
cyclicSchema =
  { name: "cyclic-fixture"
  , tables:
      [ { name: "thing_a"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "b_ref", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys:
            [ { columns: [ "b_ref" ]
              , refTable: "thing_b"
              , refColumns: [ "id" ]
              , onDelete: NoAction
              , onUpdate: NoAction
              }
            ]
        , uniqueConstraints: []
        }
      , { name: "thing_b"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "a_ref", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys:
            [ { columns: [ "a_ref" ]
              , refTable: "thing_a"
              , refColumns: [ "id" ]
              , onDelete: NoAction
              , onUpdate: NoAction
              }
            ]
        , uniqueConstraints: []
        }
      ]
  }

main :: Effect Unit
main = do
  Console.log "=== Smoke 1: well-formed tree (projects → snapshots → packages) ==="
  let okAls = generate smokeSchema
  Console.log okAls
  FS.writeTextFile UTF8 "/tmp/minard-smoke-ok.als" okAls
  Console.log ""
  Console.log "=== Smoke 2: deliberate cycle (thing_a ↔ thing_b) ==="
  let badAls = generate cyclicSchema
  Console.log badAls
  FS.writeTextFile UTF8 "/tmp/minard-smoke-bad.als" badAls
  Console.log ""
  Console.log "Run both with:"
  Console.log "  java -jar vendor/alloy.jar exec /tmp/minard-smoke-ok.als"
  Console.log "  java -jar vendor/alloy.jar exec /tmp/minard-smoke-bad.als"
  Console.log "Expect NoFKCycle: UNSAT (ok), SAT (bad)."
