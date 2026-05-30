# yoga-postgres dependency harmonization — note for Mark

**TL;DR — this is a two-line `spago.yaml` bounds bump, and it's provably
source-compatible.** `yoga-postgres` 0.3.0 still pins the *old*
`yoga-sql-types` / `heterogeneous` line, so it can't co-install in any
workspace or package set that also carries `yoga-sql-types ≥ 0.2` or
`heterogeneous ≥ 0.7` (i.e. recent registry sets). But every module
yoga-postgres actually consumes from those two packages is byte-for-byte
identical across the version jump — so widening the bounds needs no code
changes.

## The skew

`yoga-postgres` 0.3.0 `spago.yaml`:

```yaml
- heterogeneous: ">=0.6.0 <0.7.0"
- yoga-sql-types: ">=0.1.0 <0.2.0"
```

But `yoga-sql-types` 0.2.0 moved up to `heterogeneous >=0.7.0 <0.8.0`.
So yoga-postgres is internally consistent only on the *old* line
(`yoga-sql-types` 0.1.x → `heterogeneous` 0.6.x). It cannot be installed
next to the current `yoga-sql-types` 0.2 / `heterogeneous` 0.7 that
recent package sets ship. Each of the three repos uses the solver
(no package-set pin) and locks `heterogeneous` 0.6.0 (yoga-postgres,
yoga-postgres-om) or 0.7.0 (yoga-sql-types standalone) accordingly.

## Why it's a no-op for the source

yoga-postgres' entire surface area on these two packages:

**heterogeneous** — one import, one class:

```purescript
-- src/Yoga/Postgres/TypedQuery.purs:11
import Heterogeneous.Folding (class HFoldlWithIndex)
```

`Heterogeneous.Folding` is **byte-identical** between heterogeneous
0.6.0 and 0.7.0 (`diff` is empty). The only change anywhere in
heterogeneous 0.6 → 0.7 is an *additive* new module
(`Heterogeneous/Variadic.purs`). `HFoldlWithIndex` itself is unchanged:

```purescript
class HFoldlWithIndex f x a b | f x a -> b where
  hfoldlWithIndex :: f -> x -> a -> b
```

**yoga-sql-types** — one import line:

```purescript
-- src/Yoga/Postgres/TypedQuery.purs:13
import Yoga.SQL.PostgresTypes
  (class ToSQLParam, SQLParameter, SQLQuery, TurnIntoSQLParam, argsFor, sqlQueryToString)
```

Both modules of yoga-sql-types (`Yoga.SQL.Types`, `Yoga.SQL.PostgresTypes`)
are **byte-identical** between 0.1.0 and 0.2.0 (`diff` is empty). The
0.1 → 0.2 bump changed yoga-sql-types' *own* `heterogeneous` bound, not
any API yoga-postgres relies on.

## The change

In `yoga-postgres/spago.yaml`:

```diff
-    - heterogeneous: ">=0.6.0 <0.7.0"
+    - heterogeneous: ">=0.7.0 <0.8.0"
-    - yoga-sql-types: ">=0.1.0 <0.2.0"
+    - yoga-sql-types: ">=0.2.0 <0.3.0"
```

…then regenerate the lock. Cut a `0.3.1` (or `0.4.0`). No source edits.
(If you'd rather stay maximally permissive, `heterogeneous ">=0.6.0
<0.8.0"` and `yoga-sql-types ">=0.1.0 <0.3.0"` also work, since the
consumed surface is identical across both lines — but the forward pin is
cleaner for current sets.)

## Why we're asking

notothenia (a schema-cartography + Alloy-proof tool;
`CodeExplorer/minard-db`) has a bidirectional bridge to yoga's type-level
`Table`/`Q`: it generates yoga `Table` types from a parsed/introspected
schema and parses them back for drift detection. The next step
("migration-breaks-a-query becomes a type error", "compile-time dead-column
detection") wants a small **continuously-built** PureScript package that
depends on `yoga-postgres` alongside the current toolchain. Today that
package can't resolve because of the skew above; with the bump it's a
clean workspace dependency.

Happy to open the PR if useful — it's the diff above plus a lockfile
regen.

---
*Findings produced 2026-05-30 by diffing the cached package sources
(`heterogeneous` 0.6.0 vs 0.7.0; `yoga-sql-types` 0.1.0 vs 0.2.0) against
yoga-postgres 0.3.0's actual imports.*
