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
import MinardDB.Alloy.Receipt (CommandResult, parseReceipt)
import MinardDB.Properties (AlloyCheck, defaultProperties, interpretCommand)
import MinardDB.Schema (FDSource(..), FKAction(..), PGType(..), Schema)
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
        , functionalDependencies: []
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
        , functionalDependencies: []
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
        , functionalDependencies: []
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
        , functionalDependencies: []
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
        , functionalDependencies: []
        }
      ]
  }

main :: Effect Unit
main = launchAff_ do
  runOne "TREE  " smokeSchema
  Console.log ""
  runOne "CYCLE " cyclicSchema
  Console.log ""
  runOne "DENORM" denormalizedSchema

-- | Single-table schema with the classic `zip → city` BCNF violation.
-- |
-- | Why this is a violation: BCNF requires that for every non-trivial FD
-- | X → Y, X is a superkey. The PK of `addresses` is `id`; `zip` is not
-- | a key and there's no UNIQUE on it, so two distinct rows can share a
-- | zip — meaning `zip` doesn't functionally determine `city` via the
-- | schema's own constraints. Alloy will find two rows sharing a zip but
-- | disagreeing on city, breaking the BCNF assertion.
-- |
-- | Real-world fix: split into `addresses(id, zip, street)` +
-- | `zips(zip PK, city)`.
denormalizedSchema :: Schema
denormalizedSchema =
  { name: "minard-smoke-denormalized"
  , tables:
      [ { name: "addresses"
        , schemaName: "main"
        , columns:
            [ { name: "id", dataType: PGInt, nullable: false, defaultExpr: Nothing }
            , { name: "zip", dataType: PGText, nullable: false, defaultExpr: Nothing }
            , { name: "city", dataType: PGText, nullable: false, defaultExpr: Nothing }
            , { name: "street", dataType: PGText, nullable: false, defaultExpr: Nothing }
            ]
        , primaryKey: [ "id" ]
        , foreignKeys: []
        , uniqueConstraints: []
        , functionalDependencies:
            [ { determinant: [ "zip" ]
              , dependent: [ "city" ]
              , source: Declared
              }
            ]
        }
      ]
  }

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
      let catalog = defaultProperties >>= (_ $ schema)
      Console.log $ "[" <> label <> "] " <> show (Array.length cmds) <>
        " commands, exit " <> show result.exitCode <> ":"
      Console.log $ "  " <> formatHeader
      traverse_ (Console.log <<< ("  " <> _) <<< formatRow catalog) cmds

formatHeader :: String
formatHeader =
  padR 24 "command"
    <> padR 8 "kind"
    <> padR 8 "verdict"
    <> "interpretation"

formatRow :: Array AlloyCheck -> CommandResult -> String
formatRow catalog r =
  padR 24 r.name
    <> padR 8 (show r.kind)
    <> padR 8 (show r.verdict)
    <> interpretCommand (Array.find (\c -> c.name == r.name) catalog) r

padR :: Int -> String -> String
padR n s =
  let
    len = String.length s
    pad = if len >= n then "" else String.joinWith "" (Array.replicate (n - len) " ")
  in
    s <> pad
