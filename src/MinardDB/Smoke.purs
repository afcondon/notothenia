module MinardDB.Smoke where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.String as String
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class.Console as Console
import MinardDB.Alloy.Generate (generate)
import MinardDB.Alloy.Invoke (defaultConfig, runAlloy)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..), parseReceipt)
import MinardDB.Schema (FKAction(..), PGType(..), Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

-- | Tree schema (projects → snapshots → packages). NoFKCycle should hold.
smokeSchema :: Schema
smokeSchema =
  { name: "minard-smoke-ok"
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

-- | Deliberate FK cycle: thing_a → thing_b → thing_a. NoFKCycle should fail.
cyclicSchema :: Schema
cyclicSchema =
  { name: "minard-smoke-bad"
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
main = launchAff_ do
  runOne "TREE " smokeSchema
  Console.log ""
  runOne "CYCLE" cyclicSchema

runOne :: String -> Schema -> Aff Unit
runOne label schema = do
  let alsPath = "/tmp/" <> schema.name <> ".als"
  FS.writeTextFile UTF8 alsPath (generate schema)
  Console.log $ "[" <> label <> "] wrote " <> alsPath <> ", running Alloy…"
  result <- runAlloy defaultConfig alsPath
  let receiptPath = Path.concat [ schema.name, "receipt.json" ]
  receiptText <- FS.readTextFile UTF8 receiptPath
  case parseReceipt receiptText of
    Left err ->
      Console.log $ "[" <> label <> "] receipt parse error: " <> err
    Right cmds -> do
      Console.log $ "[" <> label <> "] " <> show (Array.length cmds) <>
        " commands, exit " <> show result.exitCode <> ":"
      Console.log $ "  " <> formatHeader
      traverse_ (Console.log <<< ("  " <> _) <<< formatRow) cmds

formatHeader :: String
formatHeader =
  padR 24 "command"
    <> padR 8 "kind"
    <> padR 8 "verdict"
    <> "interpretation"

formatRow :: CommandResult -> String
formatRow r =
  padR 24 r.name
    <> padR 8 (show r.kind)
    <> padR 8 (show r.verdict)
    <> interpretation r

interpretation :: CommandResult -> String
interpretation r = case r.kind, r.verdict of
  Check, NoCounterexample -> "PROVEN (no counterexample within scope)"
  Check, Counterexample -> "BROKEN (counterexample exists)"
  Run, Counterexample -> "instance found"
  Run, NoCounterexample -> "no satisfying instance"

padR :: Int -> String -> String
padR n s =
  let
    len = String.length s
    pad = if len >= n then "" else String.joinWith "" (Array.replicate (n - len) " ")
  in
    s <> pad
