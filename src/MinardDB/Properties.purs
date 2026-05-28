module MinardDB.Properties
  ( Property
  , AlloyCheck
  , CheckBody(..)
  , defaultProperties
  , noFKCycle
  , bcnf
  , coverage
  , renderCheck
  , defaultScope
  , interpretCommand
  ) where

import Prelude

import Data.Array (intercalate, null, nubBy)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import MinardDB.Alloy.Names (fieldName, sigName)
import MinardDB.Alloy.Receipt (CommandKind(..), CommandResult, Verdict(..))
import MinardDB.Schema (FDSource(..), FunctionalDependency, Schema, Table)

-- | A property generates zero or more Alloy assert/check command pairs
-- | for a given schema.
-- |
-- | Returning multiple checks from one property enables **fault
-- | localization**: BCNF emits one check per declared FD so the receipt
-- | identifies *which* FD violates BCNF, not just "BCNF failed".
type Property = Schema -> Array AlloyCheck

type AlloyCheck =
  { name :: String   -- e.g. "NoFKCycle" or "BCNF_users_zip_city"
  , scope :: Int
  , body :: CheckBody
  }

-- | The body of an Alloy command.
-- |
-- | `Assertion` compiles to `assert X { … } / check X for N` — SAT means
-- | counterexample (property fails), UNSAT means no counterexample within
-- | scope (property holds).
-- |
-- | `Vacuous reason` is used when the property has nothing applicable to
-- | check (NoFKCycle on a schema with no FK fields, BCNF on a table with
-- | no declared FDs). It compiles to `assert X { no none }` so Alloy
-- | proves it trivially — yielding a real PROVEN verdict in the receipt,
-- | with the reason captured as a comment for provenance. Preferable to
-- | emitting nothing, which would be indistinguishable from a generator
-- | bug.
-- |
-- | `Probe` is the QC-`classify` analogue: a `pred X { … } / run X for N`
-- | command whose verdict reports whether the validity facts admit an
-- | instance of a given *shape*. SAT means the shape is realizable within
-- | scope (good — the proof catalog has been exercised against that shape);
-- | UNSAT means the schema's own constraints rule the shape out at this
-- | scope. Unlike an Assertion, a Probe's "instance found" outcome is the
-- | informational result, not a violation.
data CheckBody
  = Assertion String
  | Vacuous String
  | Probe String

-- | Default scope: clamped to [3, 8] based on table count.
defaultScope :: Schema -> Int
defaultScope schema = max 3 (min 8 (Array.length schema.tables + 2))

-- | Interpretation string for one Alloy command result, derived from
-- | the AlloyCheck body kind. An `Assertion` returning UNSAT is PROVEN;
-- | a `Probe` returning SAT means a shape was realized within scope; a
-- | `Vacuous` UNSAT carries the explanation so the receipt records *why*
-- | the property was tautologically true. Commands not in the catalog
-- | (the synthetic `show` run) fall back on raw kind+verdict wording.
interpretCommand :: Maybe AlloyCheck -> CommandResult -> String
interpretCommand mcheck cmd = case mcheck of
  Just c -> case c.body, cmd.verdict of
    Assertion _, NoCounterexample -> "PROVEN (no counterexample within scope)"
    Assertion _, Counterexample -> "BROKEN (counterexample exists)"
    Vacuous reason, NoCounterexample -> "PROVEN vacuously (" <> reason <> ")"
    Vacuous _, Counterexample -> "anomaly: vacuous check returned SAT"
    Probe _, Counterexample -> "shape realized within scope"
    Probe _, NoCounterexample -> "shape NOT realized within scope"
  Nothing -> case cmd.kind, cmd.verdict of
    Run, Counterexample -> "instance found"
    Run, NoCounterexample -> "no satisfying instance"
    Check, NoCounterexample -> "PROVEN (no counterexample within scope)"
    Check, Counterexample -> "BROKEN (counterexample exists)"

-- | Render an AlloyCheck as final `.als` text.
renderCheck :: AlloyCheck -> String
renderCheck c = case c.body of
  Assertion body ->
    "assert " <> c.name <> " {\n  " <> body <> "\n}\n"
      <> "check " <> c.name <> " for " <> show c.scope
  Vacuous reason ->
    "// " <> c.name <> " is vacuous: " <> reason <> "\n"
      <> "assert " <> c.name <> " { no none }\n"
      <> "check " <> c.name <> " for " <> show c.scope
  Probe body ->
    "pred " <> c.name <> " {\n  " <> body <> "\n}\n"
      <> "run " <> c.name <> " for " <> show c.scope

-- | The default property catalog (grows as Phase 2 progresses).
defaultProperties :: Array Property
defaultProperties = [ noFKCycle, bcnf, coverage ]

--------------------------------------------------------------------------
-- NoFKCycle: no row is in its own transitive closure across any FK chain
--------------------------------------------------------------------------

noFKCycle :: Property
noFKCycle schema =
  let
    qualified = collectQualifiedFKFields schema
    scope = defaultScope schema
  in
    if null qualified then
      [ { name: "NoFKCycle"
        , scope
        , body: Vacuous "no FK fields; acyclicity trivially holds"
        }
      ]
    else
      let
        rendered = map (\{ sig, field } -> "(" <> sig <> " <: " <> field <> ")") qualified
        unioned = intercalate " + " rendered
      in
        [ { name: "NoFKCycle"
          , scope
          , body: Assertion ("no a: univ | a in a.^(" <> unioned <> ")")
          }
        ]

-- | Collect (sig, field) pairs for each FK, deduped.
collectQualifiedFKFields :: Schema -> Array { sig :: String, field :: String }
collectQualifiedFKFields schema =
  schema.tables
    # Array.concatMap
        ( \t -> Array.mapMaybe
            ( \fk -> map (\c -> { sig: sigName t.name, field: fieldName c })
                (Array.head fk.columns)
            )
            t.foreignKeys
        )
    # nubBy (\a b -> compare a.sig b.sig <> compare a.field b.field)

--------------------------------------------------------------------------
-- BCNF: for every non-trivial declared FD X → Y on table T, X must be
-- a superkey of T (no two distinct rows agree on every column of X).
--
-- One check is emitted per FD so the receipt names which FD violates BCNF
-- (fault localization). Skips:
--   - trivial FDs (Y ⊆ X)
--   - FDs whose determinant contains a PK column — those are trivially
--     BCNF-satisfying (PK is already a superkey; supersets stay superkeys)
--     and aren't directly expressible in Alloy anyway because PK columns
--     are encoded as atom identity, not as accessible fields.
--
-- If no non-trivial requiring-proof FDs remain across the whole schema,
-- emits a single vacuous PROVEN check so the receipt records that BCNF
-- was evaluated.
--------------------------------------------------------------------------

bcnf :: Property
bcnf schema =
  let
    scope = defaultScope schema
    checks = schema.tables >>= bcnfChecksForTable
  in
    if null checks then
      [ { name: "BCNF"
        , scope
        , body: Vacuous "no non-trivial declared FDs requiring proof"
        }
      ]
    else
      map (\c -> c { scope = scope }) checks

bcnfChecksForTable :: Table -> Array AlloyCheck
bcnfChecksForTable t =
  let
    pkSet = Set.fromFoldable t.primaryKey
    declared = Array.filter (\fd -> fd.source == Declared) t.functionalDependencies
  in
    Array.mapMaybe (tryBuildBCNFCheck t pkSet) declared

tryBuildBCNFCheck
  :: Table
  -> Set.Set String
  -> FunctionalDependency
  -> Maybe AlloyCheck
tryBuildBCNFCheck t pkSet fd =
  let
    detSet = Set.fromFoldable fd.determinant
    depSet = Set.fromFoldable fd.dependent
    trivial = depSet `Set.subset` detSet
    detIntersectsPK = not (Set.isEmpty (Set.intersection detSet pkSet))
  in
    if trivial || detIntersectsPK then Nothing
    else Just (buildBCNFCheck t fd)

buildBCNFCheck :: Table -> FunctionalDependency -> AlloyCheck
buildBCNFCheck t fd =
  let
    name = "BCNF_" <> sigName t.name <> "_"
      <> intercalate "_" (map fieldName fd.determinant)
      <> "__"
      <> intercalate "_" (map fieldName fd.dependent)
    sig = "this/" <> sigName t.name
    comparisons = map (\c -> "a." <> fieldName c <> " != b." <> fieldName c) fd.determinant
    body = "all disj a, b: " <> sig <> " | " <> intercalate " or " comparisons
  in
    { name
    , scope: 0  -- replaced by caller (bcnf above)
    , body: Assertion body
    }

--------------------------------------------------------------------------
-- Coverage probes: the `classify` analogue from the QuickCheck family.
--
-- A property test that never exercised the shapes it's supposed to defend
-- against is a paper proof. QuickCheck's `classify` reports the
-- distribution of input shapes that ran through a property; Hughes &
-- Claessen ship it for exactly this reason. The Alloy translation is:
-- emit a `run` command for each interesting shape and let the verdict
-- (SAT/UNSAT) tell us whether the validity facts admit that shape within
-- scope.
--
-- Output reads like: "checked at scope 8 — schema admits acyclic
-- populations ✓, schema admits cyclic populations ✓, schema admits
-- multi-hop FK chains ✓" alongside the assert verdicts.
--
-- For schemas with no FK fields the shapes are vacuous; we emit one
-- vacuous AcyclicShape so the receipt records that coverage ran.
--------------------------------------------------------------------------

coverage :: Property
coverage schema =
  let
    scope = defaultScope schema
    qualified = collectQualifiedFKFields schema
  in
    if null qualified then
      [ { name: "AcyclicShape"
        , scope
        , body: Vacuous "no FK fields; FK-graph shape probes are inapplicable"
        }
      ]
    else
      let
        rendered = map (\{ sig, field } -> "(" <> sig <> " <: " <> field <> ")") qualified
        unioned = intercalate " + " rendered
      in
        [ { name: "AcyclicShape"
          , scope
          , body: Probe ("no a: univ | a in a.^(" <> unioned <> ")")
          }
        , { name: "CyclicShape"
          , scope
          , body: Probe ("some a: univ | a in a.^(" <> unioned <> ")")
          }
        -- ChainOfLength2: at least one 2-step FK path exists.
        -- We can't write `some a.(R).(R)` directly: when the same FK
        -- column name is reused across tables (e.g. `package_version_id`
        -- appears in five tables of ce-unified), Alloy's resolver chokes
        -- on the second dot-join even though each summand is qualified
        -- with `<:`. The wrapped form `a.^(R)` works because the closure
        -- operator promotes the union into a single typed relation. Three
        -- explicit atoms get us a two-step chain without depending on
        -- relation composition.
        , { name: "ChainOfLength2"
          , scope
          , body: Probe
              ( "some a, b, c: univ | b in a.(" <> unioned <> ")"
                  <> " and c in b.(" <> unioned <> ")"
              )
          }
        ]
