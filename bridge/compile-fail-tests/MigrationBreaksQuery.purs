-- EXPECT: NoInstanceFound
-- |
-- | Rung 4 — the **must-not-compile half** of migration-breaks-a-query.
-- |
-- | `userContacts` reaches the `email` column. Against schema **V2** —
-- | where the migration dropped `email` — yoga's `select @"name, email"`
-- | can't resolve `email` on the table type, so `ResolveColumn` has no
-- | instance and the compiler stops *here*, at this query, with
-- | `NoInstanceFound`. Not a runtime error, not a failed test against a
-- | live database — a type error at the exact call site of the one query
-- | the migration invalidated.
-- |
-- | This is what makes a column drop *safe to perform*: every query that
-- | silently depended on the dropped column becomes a compile error you
-- | must address before the build is green again. Contrast `userAgesV2`
-- | in `Minard.Bridge.Migration.Queries`, which reaches no dropped column
-- | and keeps compiling.
-- |
-- | Run via `bridge/compile-fail-tests/run.sh`. The harness copies this
-- | file into `bridge/src/`, builds `minard-bridge`, and asserts the
-- | build fails carrying the `-- EXPECT:` substring above.
module Minard.Bridge.CompileFail.MigrationBreaksQuery where

import Data.Function ((#))
import Yoga.Postgres.Schema (from, select, where_)
import Minard.Bridge.Migration.SchemaV2 as V2

-- Reaches `email`, which V2 no longer has. Does not compile.
userContacts = from V2.usersTable # select @"name, email" # where_ @"id = $id"
