-- | Per-step referential-integrity safety analysis over a migration
-- | trace.
-- |
-- | The static pass walks the trace produced by
-- | `MinardDB.Migration.runSequence` and reports issues introduced by
-- | each step. "Introduced" is computed as a set-difference between
-- | `before` and `after` issue sets, so a step that *removes* a
-- | pre-existing issue (e.g. a CreateTable that fixes a dangling FK
-- | added earlier) doesn't get blamed for issues it had nothing to do
-- | with.
-- |
-- | This is the static layer; an Alloy 6 temporal layer
-- | (`MinardDB.Migration.Alloy`, Phase 3b) will sit on top to prove
-- | temporal properties like "RI is preserved across every reachable
-- | step" with bounded counterexamples when it's not.
module MinardDB.Migration.Safety
  ( Issue(..)
  , StepReport
  , SafetyReport
  , describeIssue
  , checkSchemaRI
  , checkStep
  , runSafetyReport
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (foldMap)
import Data.Maybe (Maybe(..))
import Data.String (joinWith)
import MinardDB.Migration (Migration, TraceStep)
import MinardDB.Schema (ForeignKey, Schema, Table)

-- | A referential-integrity issue in a schema state. Each constructor
-- | identifies the *table that owns the broken FK*, the FK itself, and
-- | the specific reason. We carry the FK by value so the report can
-- | render it verbatim (column names, target table) without re-looking
-- | it up.
data Issue
  = MissingTargetTable
      { table :: String         -- the table that owns the FK
      , fk :: ForeignKey
      }
  | MissingTargetColumn
      { table :: String         -- the table that owns the FK
      , fk :: ForeignKey
      , refColumn :: String     -- the missing column on `fk.refTable`
      }

derive instance eqIssue :: Eq Issue

describeIssue :: Issue -> String
describeIssue = case _ of
  MissingTargetTable r ->
    "DanglingFK: " <> r.table <> "(" <> joinWith ", " r.fk.columns
      <> ") -> " <> r.fk.refTable <> " (target table does not exist)"
  MissingTargetColumn r ->
    "DanglingFK: " <> r.table <> "(" <> joinWith ", " r.fk.columns
      <> ") -> " <> r.fk.refTable <> "(" <> r.refColumn
      <> ") (target column does not exist)"

-- | Whole-schema RI scan: for every FK on every table, ensure the
-- | target table exists and every referenced column exists on it.
-- | Order of returned issues is stable (table iteration order, then FK
-- | iteration order, then refColumn iteration order) so set-difference
-- | for "newly introduced" works on plain Array equality.
checkSchemaRI :: Schema -> Array Issue
checkSchemaRI s = s.tables # foldMap \t ->
  t.foreignKeys # foldMap \fk -> checkOneFK s t fk

checkOneFK :: Schema -> Table -> ForeignKey -> Array Issue
checkOneFK s t fk = case lookupTable fk.refTable s of
  Nothing -> [ MissingTargetTable { table: t.name, fk } ]
  Just target ->
    fk.refColumns # foldMap \rc ->
      if hasColumn rc target then []
      else [ MissingTargetColumn { table: t.name, fk, refColumn: rc } ]

------------------------------------------------------------------------
-- Trace walking
------------------------------------------------------------------------

type StepReport =
  { migration :: Migration
  , before :: Schema
  , after :: Schema
  , introduced :: Array Issue   -- issues present in `after` but not `before`
  , resolved :: Array Issue     -- issues present in `before` but not `after`
  , standing :: Array Issue     -- still-broken FKs as of `after`
  }

type SafetyReport =
  { steps :: Array StepReport
  , finalStanding :: Array Issue
  , totalIntroduced :: Int
  }

-- | Diff two issue lists. Cheap O(n*m) because issue lists are tiny
-- | in practice (a counterexample-finder is supposed to surface the
-- | first one).
diffIssues :: Array Issue -> Array Issue -> Array Issue
diffIssues from to = Array.filter (\i -> not (Array.elem i from)) to

checkStep :: TraceStep -> StepReport
checkStep step =
  let
    before = checkSchemaRI step.before
    after = checkSchemaRI step.after
  in
    { migration: step.migration
    , before: step.before
    , after: step.after
    , introduced: diffIssues before after
    , resolved: diffIssues after before
    , standing: after
    }

runSafetyReport :: Array TraceStep -> SafetyReport
runSafetyReport trace =
  let
    steps = map checkStep trace
    finalStanding = case Array.last steps of
      Just last -> last.standing
      Nothing -> []
    totalIntroduced = Array.length (Array.concatMap _.introduced steps)
  in
    { steps, finalStanding, totalIntroduced }

------------------------------------------------------------------------
-- helpers (small enough to inline; pulled out for readability)
------------------------------------------------------------------------

lookupTable :: String -> Schema -> Maybe Table
lookupTable n s = Array.find (\t -> t.name == n) s.tables

hasColumn :: String -> Table -> Boolean
hasColumn n t = Array.any (\c -> c.name == n) t.columns

