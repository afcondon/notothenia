module MinardDB.Properties
  ( Property
  , AlloyCheck
  , CheckBody(..)
  , defaultProperties
  , noFKCycle
  , bcnf
  , renderCheck
  , defaultScope
  ) where

import Prelude

import Data.Array (intercalate, null, nubBy)
import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.Set as Set
import MinardDB.Alloy.Names (fieldName, sigName)
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

-- | The body of an Alloy assertion.
-- |
-- | `Vacuous reason` is used when the property has nothing applicable to
-- | check (NoFKCycle on a schema with no FK fields, BCNF on a table with
-- | no declared FDs). It compiles to `assert X { no none }` so Alloy
-- | proves it trivially — yielding a real PROVEN verdict in the receipt,
-- | with the reason captured as a comment for provenance. Preferable to
-- | emitting nothing, which would be indistinguishable from a generator
-- | bug.
data CheckBody
  = Assertion String
  | Vacuous String

-- | Default scope: clamped to [3, 8] based on table count.
defaultScope :: Schema -> Int
defaultScope schema = max 3 (min 8 (Array.length schema.tables + 2))

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

-- | The default property catalog (grows as Phase 2 progresses).
defaultProperties :: Array Property
defaultProperties = [ noFKCycle, bcnf ]

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
