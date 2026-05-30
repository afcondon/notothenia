-- | Schema **V1** — the "before" of a migration, as notothenia's rung-1
-- | codegen (`MinardDB.Codegen.YogaTable.emitModule`) emits it from a
-- | `CREATE TABLE`. A yoga `Table` type IS the type-level schema the
-- | compiler checks queries against.
-- |
-- | V1 has an `email` column. The migration to `SchemaV2` drops it. The
-- | point of rung 4 (see `bridge/RUNG4.md`): a query that *reaches*
-- | `email` compiles here and stops compiling against V2 — at its exact
-- | call site, with a precise `NoInstanceFound` error. That is a
-- | migration-safety check the type system performs for free.
module Minard.Bridge.Migration.SchemaV1 where

import Type.Proxy (Proxy(..))
import Yoga.Postgres.Schema (Table, PrimaryKey, Nullable)

type UsersTable = Table "users"
  ( id :: PrimaryKey Int
  , name :: String
  , email :: String
  , age :: Nullable Int
  )

usersTable :: Proxy UsersTable
usersTable = Proxy
