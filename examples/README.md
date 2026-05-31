# notothenia examples

Schemas you can run `notothenia check` against. The `shop` family exists
to give the **Alloy audit tier something real to bite on** — see below.

## Why these exist: the vacuity problem

notothenia's two tiers are the fast pure-PureScript lints and the Alloy
**audit tier** (bounded model finding of relational properties). The
audit tier can only find a counterexample if the schema actually carries
the structure the property is about:

- **`NoFKCycle`** needs real foreign keys. The DuckDB-introspected
  examples don't have them — DuckDB ships with FK enforcement disabled,
  so catalog introspection finds no FKs and acyclicity passes *vacuously*
  (nothing to cycle).
- **`BCNF`** needs functional dependencies. No relational catalog stores
  non-key FDs, so without declared FDs (via an `.intent` file) BCNF also
  passes vacuously ("no non-trivial declared FDs requiring proof").

A vacuous PASS looks identical to a real one in a green checkmark, but
it proves nothing. The `shop` schemas are Postgres-dialect DDL with a
genuine multi-table FK graph and declared FDs, so every audit property
has content — and we can show it failing as well as passing.

## The `shop` family

A normalized storefront: `customers`, `categories`, `products`,
`orders`, `order_items`, with an acyclic five-table FK graph.

| File | What it shows | Verdict |
|------|---------------|---------|
| `shop.sql` + `shop.intent` | Healthy schema. `NoFKCycle` **PROVEN** *with bite* — the `CyclicShape` coverage probe comes back **NOT realized**, so the constraints actively rule cycles out. `BCNF` **PROVEN** per declared candidate-key FD. | clean / exit 0 |
| `shop-cyclic.sql` | One added FK (`customers.featured_order_id → orders`) closes a real cross-table cycle `customers → orders → customers`. `NoFKCycle` **BROKEN**; `CyclicShape` now **realized**. | fail / exit ≠ 0 |
| `shop-denormalized.sql` + `shop-denormalized.intent` | A denormalized `shipments` table with the textbook `zip → city, state` dependency (ZIP is not a key). `BCNF` **BROKEN**, fault-localized to `BCNF_shipments_zip__city_state`; the candidate-key FDs still pass; `NoFKCycle` still PROVEN. | fail / exit ≠ 0 |

The `shop.sql` ↔ `shop-cyclic.sql` pair is the key contrast: the same
property comes back PROVEN-with-bite on one and BROKEN on the other, so
the green checkmark is earned, not vacuous.

### Run them

```bash
# Healthy — all audit properties PROVEN non-vacuously
spago run -p minard-db --main MinardDB.Check -- \
    --sql examples/shop.sql --intent examples/shop.intent --alloy

# Real multi-table FK cycle — NoFKCycle BROKEN
spago run -p minard-db --main MinardDB.Check -- \
    --sql examples/shop-cyclic.sql --alloy

# BCNF violation — BROKEN, fault-localized to the offending FD
spago run -p minard-db --main MinardDB.Check -- \
    --sql examples/shop-denormalized.sql --intent examples/shop-denormalized.intent --alloy
```

(Drop `--alloy` to run only the fast tier; add `--json` for a
machine-readable verdict and a CI-friendly exit code.)

## `blog` — the intent-layer example

`blog.sql` + `blog.intent` is the smaller example for the **intent
layer**: a `parent_id` reply-tree FK that a "legacy migration" dropped
from the DDL, re-declared in the `.intent` file, plus `waive`
directives that acknowledge the append-only reply tree so the
self-referential cycle isn't reported as a bug. It demonstrates
`fk`/`waive` intent directives rather than non-vacuous audit proofs.

## Live Postgres (not yet wired)

These examples are Postgres-dialect *DDL text*, parsed by
`MinardDB.Migration.SQL`. Pointing notothenia at a **live** Postgres
(its `pg_catalog` / `information_schema`) is the natural next step — a
`tools/introspect-postgres.py` mirroring `tools/introspect-duckdb.py`,
emitting the same Schema-AST JSON, plus a `--schema-json` ingest option
on `check`. A live Postgres restores real FKs without the DuckDB
disabled-FK caveat.
