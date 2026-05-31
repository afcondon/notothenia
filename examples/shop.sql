-- A normalized Postgres schema with a genuine multi-table foreign-key
-- DAG — the kind of substrate the Alloy audit tier was built for, and
-- which the DuckDB-introspected examples can't provide (DuckDB ships
-- with FK enforcement disabled, so introspection finds no FKs and every
-- relational proof comes back vacuous).
--
-- The FK graph here is acyclic and spans five tables:
--
--     order_items ──▶ orders ──▶ customers
--          │
--          └────────▶ products ──▶ categories
--
-- so `NoFKCycle` is PROVEN with *bite*: the coverage probe `CyclicShape`
-- comes back unrealizable, meaning the constraints actively rule cycles
-- out — not "we happened not to find one". Contrast examples/shop-cyclic.sql.
--
-- Run:
--   spago run -p minard-db --main MinardDB.Check -- \
--       --sql examples/shop.sql --intent examples/shop.intent --alloy

CREATE TABLE customers (
  id    INTEGER PRIMARY KEY,
  email TEXT NOT NULL UNIQUE,
  name  TEXT NOT NULL
);

CREATE TABLE categories (
  id   INTEGER PRIMARY KEY,
  name TEXT NOT NULL UNIQUE
);

CREATE TABLE products (
  id          INTEGER PRIMARY KEY,
  category_id INTEGER NOT NULL REFERENCES categories(id),
  sku         TEXT NOT NULL UNIQUE,
  name        TEXT NOT NULL,
  price_cents INTEGER NOT NULL
);

CREATE TABLE orders (
  id          INTEGER PRIMARY KEY,
  customer_id INTEGER NOT NULL REFERENCES customers(id),
  placed_at   TIMESTAMP NOT NULL
);

CREATE TABLE order_items (
  order_id   INTEGER NOT NULL REFERENCES orders(id),
  product_id INTEGER NOT NULL REFERENCES products(id),
  quantity   INTEGER NOT NULL,
  PRIMARY KEY (order_id, product_id)
);
