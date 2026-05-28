module MinardDB.Alloy.Minimize
  ( minimizeScope
  ) where

import Prelude

import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import MinardDB.Alloy.Generate (generateWithChecks)
import MinardDB.Alloy.Invoke (AlloyConfig, runAlloy)
import MinardDB.Alloy.Receipt (Verdict(..), parseReceipt)
import MinardDB.Properties (AlloyCheck)
import MinardDB.Schema (Schema)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS
import Node.Path as Path

-- | Given an AlloyCheck that returned SAT at its current scope, find the
-- | smallest scope at which it still returns SAT. This is the shrinking
-- | analogue from the QuickCheck family — a 2-row counterexample is far
-- | more debuggable than an 8-row one.
-- |
-- | Returns `Just minScope` where minScope ≤ check.scope is the smallest
-- | scope in [1, check.scope] that still produces a SAT verdict.
-- | Returns `Nothing` only on error (receipt parse failure, file IO
-- | failure, etc.) — never when the property is verifiable.
-- |
-- | Binary-searches the [1, check.scope] interval. Worst case ⌈log₂ N⌉
-- | Alloy invocations, where N is the initial scope.
minimizeScope :: AlloyConfig -> Schema -> AlloyCheck -> Aff (Maybe Int)
minimizeScope cfg schema check
  | check.scope <= 1 = pure (Just check.scope)  -- already minimal
  | otherwise = binarySearch cfg schema check 1 check.scope

-- | Invariant: `hi` is known SAT. Find the smallest SAT scope in [lo, hi].
binarySearch
  :: AlloyConfig
  -> Schema
  -> AlloyCheck
  -> Int
  -> Int
  -> Aff (Maybe Int)
binarySearch cfg schema check lo hi
  | lo >= hi = pure (Just hi)
  | otherwise = do
      let mid = (lo + hi) / 2
      verdict <- checkAtScope cfg schema check mid
      case verdict of
        Just true -> binarySearch cfg schema check lo mid           -- still SAT
        Just false -> binarySearch cfg schema check (mid + 1) hi    -- UNSAT, need bigger
        Nothing -> pure Nothing                                      -- error

-- | Run Alloy with the single check at the given scope, return whether
-- | it returned SAT (Just true) / UNSAT (Just false) / failed (Nothing).
checkAtScope :: AlloyConfig -> Schema -> AlloyCheck -> Int -> Aff (Maybe Boolean)
checkAtScope cfg schema check scope = do
  let
    modifiedCheck = check { scope = scope }
    -- Per-scope unique stem so multiple parallel minimizations don't
    -- clobber each other and so the .als files survive for debugging.
    stem = "tmp-minimize-" <> schema.name <> "-" <> check.name <> "-s" <> show scope
    alsPath = "/tmp/" <> stem <> ".als"
    alsContent = generateWithChecks [ modifiedCheck ] schema
  FS.writeTextFile UTF8 alsPath alsContent
  _ <- runAlloy cfg alsPath
  let receiptPath = Path.concat [ stem, "receipt.json" ]
  receiptText <- FS.readTextFile UTF8 receiptPath
  case parseReceipt receiptText of
    Left _ -> pure Nothing
    Right results ->
      case Array.find (\r -> r.name == check.name) results of
        Nothing -> pure Nothing
        Just r -> pure (Just (r.verdict == Counterexample))
