-- | Rung 4 — migration-breaks-a-query, the **continuously-built half**.
-- |
-- | A small manifest of typed queries, the kind an application carries
-- | against its schema. Each definition's explicit `Q` signature makes
-- | the *reach* visible: the `result` row is the projected columns, the
-- | `params` row the WHERE params typed to the columns they compare.
-- |
-- | What this module proves by *compiling green*:
-- |
-- |   * `userContactsV1` reaches `email` and type-checks against V1.
-- |   * `userAgesV1` reaches only `name`/`age` and type-checks against V1.
-- |   * `userAgesV2` — the *same query* — still type-checks against V2,
-- |     where `email` was dropped. The migration is real and the schema is
-- |     usable; queries that didn't depend on `email` are unaffected.
-- |
-- | The other half — that `userContacts` *fails to compile* against V2,
-- | pinpointed at its call site — can't live in a green build by
-- | construction, so it lives in `bridge/compile-fail-tests/`, exercised
-- | by `run.sh` with an `-- EXPECT: NoInstanceFound` marker (yoga's own
-- | compile-fail convention). Together the two halves are the rung-4
-- | check: a schema migration that drops a reached column turns into a
-- | type error at exactly the queries that reached it.
module Minard.Bridge.Migration.Queries where

import Data.Function ((#))
import Data.Unit (Unit)
import Yoga.Postgres.Schema (Q, PrimaryKey, Nullable, from, select, where_)
import Minard.Bridge.Migration.SchemaV1 as V1
import Minard.Bridge.Migration.SchemaV2 as V2

-- Reaches `email`. Compiles against V1.
userContactsV1
  :: Q
       ( users :: ( id :: PrimaryKey Int, name :: String, email :: String, age :: Nullable Int ) )
       ( name :: String, email :: String )   -- result: reach (includes email)
       ( id :: Int )                          -- params: $id
       ( select :: Unit, "where" :: Unit )
userContactsV1 = from V1.usersTable # select @"name, email" # where_ @"id = $id"

-- Reaches only `name`/`age`. Compiles against V1.
userAgesV1
  :: Q
       ( users :: ( id :: PrimaryKey Int, name :: String, email :: String, age :: Nullable Int ) )
       ( name :: String )
       ( age :: Int )
       ( select :: Unit, "where" :: Unit )
userAgesV1 = from V1.usersTable # select @"name" # where_ @"age = $age"

-- The SAME query as `userAgesV1`, now against V2 (email dropped). Still
-- compiles — the migration didn't touch any column this query reaches.
-- This is the control: it shows V2 is a valid, queryable schema, so the
-- compile failure of `userContacts` against V2 is specifically about the
-- dropped reached column, not about V2 being broken.
userAgesV2
  :: Q
       ( users :: ( id :: PrimaryKey Int, name :: String, age :: Nullable Int ) )
       ( name :: String )
       ( age :: Int )
       ( select :: Unit, "where" :: Unit )
userAgesV2 = from V2.usersTable # select @"name" # where_ @"age = $age"
