-- | Smoke test for the bridge workspace package: proves the harmonized
-- | yoga-postgres (GitHub build, deps PR #1) resolves and its type-level
-- | `Q` machinery compiles inside this workspace under package set 73.3.0.
-- |
-- | This is the foundation the rung-3/4 work sits on (see
-- | `docs/SYNTHESIS.md`): a continuously-built package depending on
-- | yoga-postgres, kept separate so `minard-db` itself stays
-- | yoga-dependency-free.
module Minard.Bridge.Smoke where

import Prelude

import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema (Q, Table, PrimaryKey, Nullable, from, select, where_)

-- A generated-style yoga `Table` type (as notothenia's rung-1 codegen
-- emits): the type-level schema the compiler checks queries against.
type UsersTable = Table "users"
  ( id :: PrimaryKey Int
  , name :: String
  , email :: String
  , age :: Nullable Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy

-- A typed query. The compiler verifies `name`/`email` exist on the table
-- and that the `$id` param matches the `id` column's type. This is the
-- exact mechanism rung 4 turns into a migration-safety check: drop a
-- reached column from the schema type and this stops compiling.
-- The signature is spelled out to show where the rung-3/4 signals live in
-- `Q tables result params stage`:
--   * `result` row = the PROJECTED columns (projection reach)
--   * `params` row = WHERE params, each typed to the column it compares
userContacts
  :: Q
       ( users :: ( id :: PrimaryKey Int, name :: String, email :: String, age :: Nullable Int ) )
       ( name :: String, email :: String )   -- result: reach
       ( id :: Int )                          -- params: $id, typed to the id column
       ( select :: Unit, "where" :: Unit )    -- stage: typestate (`where` quoted — reserved word)
userContacts = from usersTable # select @"name, email" # where_ @"id = $id"
