-- | Extension-ladder **rung 2**: run notothenia's Alloy proof catalog on
-- | a `Schema` *derived from rowtype-yoga `Table` types* — i.e. prove
-- | structural properties of the schema described by typed bindings the
-- | PureScript compiler has already checked.
-- |
-- | This is the bridge closing into the prover. Rung 1 (`YogaTable` /
-- | `YogaParse`) made the Schema AST the pivot between value-world and
-- | type-world; rung 2 feeds the type-world side back through the
-- | value-world's Alloy pipeline. The point, in the linter-at-pace
-- | framing (see `docs/SYNTHESIS.md`): **the proof catalog doesn't care
-- | where the Schema came from** — DDL, live catalog, or the typed
-- | bindings an agent just wrote. The compiler proved the *queries*
-- | conform to the types; Alloy now proves the *schema those types
-- | describe* is FK-acyclic / in BCNF. Two provers, one Schema.
-- |
-- | What is and isn't provable from yoga types (an honest rung-2 finding):
-- |
-- |   * **FK-acyclicity is fully provable.** ForeignKey…References
-- |     wrappers survive the type round-trip, so the FK graph is
-- |     reconstructed exactly and `NoFKCycle` is meaningful.
-- |   * **BCNF is structurally NOT provable from yoga types.** A yoga
-- |     `Table` type cannot express a non-key functional dependency, so
-- |     `parseYogaSchema` always yields `functionalDependencies: []` and
-- |     BCNF comes out vacuously PROVEN. This is not a notothenia bug and
-- |     not a DuckDB artifact — no relational catalog stores non-key FDs
-- |     either. Making BCNF non-vacuous needs FD inference or annotation,
-- |     on any engine.
-- |
-- | Run via:
-- |   spago run -p minard-db --main MinardDB.Codegen.YogaProve
-- |
-- | (Requires the Alloy jar on the path `MinardDB.Alloy.Invoke` expects,
-- | same as `MinardDB.Smoke`.)
module MinardDB.Codegen.YogaProve where

import Prelude

import Data.Either (Either(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console as Console
import MinardDB.Codegen.YogaParse (parseYogaSchema)
import MinardDB.Smoke (runOne)
import Node.Encoding (Encoding(..))
import Node.FS.Aff as FS

-- | The real generated Marginalia bindings — typed bindings notothenia
-- | itself emitted (rung 1) from the project tracker's schema.
generatedPath :: String
generatedPath =
  "/Users/afc/work/afc-work/CodeExplorer/minard-db/generated/MarginaliaSchema.purs"

main :: Effect Unit
main = launchAff_ do
  Console.log "── Rung 2: prove structural properties on a Schema derived from yoga `Table` *types* ──"
  Console.log "   (the compiler already checked these types; Alloy now proves properties of the schema they describe)"
  Console.log ""

  -- 1. REAL: Marginalia's generated typed bindings. Honest outcome — its
  -- FKs are disabled in DuckDB so the types carry none (NoFKCycle
  -- vacuous), and yoga types can't carry FDs (BCNF vacuous). The catalog
  -- still runs and records *why* each verdict is vacuous.
  marginaliaText <- FS.readTextFile UTF8 generatedPath
  proveFromYoga "REAL  " "yoga-marginalia" marginaliaText

  Console.log ""
  -- 2. ACYCLIC hand-written yoga with *enabled* FKs (reviews → books →
  -- authors). NoFKCycle should be PROVEN within scope.
  proveFromYoga "DAG   " "yoga-acyclic" acyclicModule

  Console.log ""
  -- 3. CYCLIC hand-written yoga (orgs ⇄ people, both NOT NULL). NoFKCycle
  -- should be BROKEN, with a 2-row counterexample.
  proveFromYoga "CYCLE " "yoga-cyclic" cyclicModule

-- | Parse a yoga module to a `Schema` (rung-1 reverse bridge), then run
-- | the existing Alloy proof driver on it (rung-2 = reuse the pipeline).
proveFromYoga :: String -> String -> String -> Aff Unit
proveFromYoga label name moduleText =
  case parseYogaSchema name moduleText of
    Left err ->
      liftEffect $ Console.log $ "[" <> label <> "] yoga parse failed — " <> err
    Right schema -> runOne label schema

------------------------------------------------------------------------
-- Hand-written yoga fixtures (in the exact form `YogaParse` accepts —
-- the vocabulary `YogaTable` emits). These stand in for "types an agent
-- wrote"; the only thing rung 2 needs from them is that they parse.
------------------------------------------------------------------------

-- | A DAG of FKs: reviews → books → authors. No cycle.
acyclicModule :: String
acyclicModule =
  """
  module YogaAcyclic where
  import Yoga.Postgres.Schema (Table, PrimaryKey, ForeignKey, References)

  type AuthorsTable = Table "authors"
    ( id :: PrimaryKey Int
    , name :: String
    )

  type BooksTable = Table "books"
    ( id :: PrimaryKey Int
    , author_id :: ForeignKey "authors" References "id" Int
    , title :: String
    )

  type ReviewsTable = Table "reviews"
    ( id :: PrimaryKey Int
    , book_id :: ForeignKey "books" References "id" Int
    , rating :: Int
    )
  """

-- | A hard 2-cycle: orgs.lead_id → people, people.org_id → orgs, both
-- | NOT NULL. NoFKCycle should fail.
cyclicModule :: String
cyclicModule =
  """
  module YogaCyclic where
  import Yoga.Postgres.Schema (Table, PrimaryKey, ForeignKey, References)

  type OrgsTable = Table "orgs"
    ( id :: PrimaryKey Int
    , lead_id :: ForeignKey "people" References "id" Int
    )

  type PeopleTable = Table "people"
    ( id :: PrimaryKey Int
    , org_id :: ForeignKey "orgs" References "id" Int
    )
  """
