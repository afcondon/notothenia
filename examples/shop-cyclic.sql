-- examples/shop.sql with one FK added that closes a genuine *cross-table*
-- cycle: a "featured order" pointer on customers, pointing back at orders,
-- which already points at customers.
--
--     customers ──▶ orders ──▶ customers        (2-cycle)
--
-- Unlike the self-referential posts→posts in examples/blog.sql, this is a
-- real multi-table cycle — the kind a DELETE policy or a migration can
-- deadlock on. The audit tier catches it: `NoFKCycle` comes back BROKEN
-- with a counterexample, where on shop.sql it was PROVEN.
--
-- The closing FK is added with ALTER TABLE because the reference is
-- mutual — Postgres can't satisfy it inline at CREATE time either.
--
-- Run:
--   spago run -p minard-db --main MinardDB.Check -- --sql examples/shop-cyclic.sql --alloy

CREATE TABLE customers (
  id                INTEGER PRIMARY KEY,
  email             TEXT NOT NULL UNIQUE,
  name              TEXT NOT NULL,
  featured_order_id INTEGER
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

-- The cycle-closing FK: customers.featured_order_id ──▶ orders.id
ALTER TABLE customers
  ADD CONSTRAINT customers_featured_order_fk
  FOREIGN KEY (featured_order_id) REFERENCES orders(id);
