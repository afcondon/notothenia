module MinardDB.Alloy.Invoke where

import Prelude

import Data.Either (Either(..))
import Effect.Aff (Aff, makeAff, nonCanceler)
import Effect.Ref as Ref
import Node.Buffer as Buffer
import Node.ChildProcess as CP
import Node.ChildProcess.Types (Exit(..))
import Node.Encoding (Encoding(..))
import Node.EventEmitter (on_)
import Node.Stream as Stream

-- | Configuration for an Alloy invocation.
type AlloyConfig =
  { javaPath :: String     -- e.g. "/opt/homebrew/opt/openjdk/bin/java"
  , alloyJar :: String     -- e.g. "vendor/alloy.jar"
  }

-- | Result of running Alloy on a single .als file.
-- | Output directory (with receipt.json) is created in cwd, named
-- | after the .als file's stem.
type AlloyResult =
  { exitCode :: Int
  , stdout :: String
  , stderr :: String
  }

-- | Default config for our repo layout (keg-only Homebrew Java).
defaultConfig :: AlloyConfig
defaultConfig =
  { javaPath: "/opt/homebrew/opt/openjdk/bin/java"
  , alloyJar: "vendor/alloy.jar"
  }

-- | Spawn `java -jar alloy.jar exec <alsPath>`, capture stdout/stderr,
-- | wait for exit.
runAlloy :: AlloyConfig -> String -> Aff AlloyResult
runAlloy cfg alsPath = makeAff \callback -> do
  -- `-f` forces overwrite of the output directory if it exists, otherwise
  -- Alloy aborts with "contains files. Delete them or use the -f option"
  -- and we'd read a stale receipt.
  cp <- CP.spawn cfg.javaPath [ "-jar", cfg.alloyJar, "exec", "-f", alsPath ]
  stdoutRef <- Ref.new ""
  stderrRef <- Ref.new ""
  CP.stdout cp # on_ Stream.dataH \chunk -> do
    str <- Buffer.toString UTF8 chunk
    Ref.modify_ (_ <> str) stdoutRef
  CP.stderr cp # on_ Stream.dataH \chunk -> do
    str <- Buffer.toString UTF8 chunk
    Ref.modify_ (_ <> str) stderrRef
  cp # on_ CP.exitH \exit -> do
    out <- Ref.read stdoutRef
    err <- Ref.read stderrRef
    let code = case exit of
          Normally n -> n
          BySignal _ -> -1
    callback (Right { exitCode: code, stdout: out, stderr: err })
  pure nonCanceler
