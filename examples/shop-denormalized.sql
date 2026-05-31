-- examples/shop.sql plus a denormalized `shipments` table that embeds a
-- postal address inline. The address carries a functional dependency the
-- schema doesn't know about: a ZIP code determines its city and state.
-- ZIP is not a key of `shipments` (many shipments share a ZIP), so this
-- is a textbook BCNF violation — the redundancy that lets two rows
-- disagree about which city a ZIP is in.
--
-- The FD is declared in examples/shop-denormalized.intent. With it, the
-- audit tier's BCNF check comes back BROKEN, fault-localized to the
-- offending dependency (`shipments: zip -> city, state`), while the
-- candidate-key FDs on customers/products still pass. The cure Alloy
-- points at is the standard decomposition: split the address out into a
-- `zip_codes(zip PK, city, state)` table that `shipments` references.
--
-- Run:
--   spago run -p minard-db --main MinardDB.Check -- \
--       --sql examples/shop-denormalized.sql --intent examples/shop-denormalized.intent --alloy

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

-- Denormalized: city/state are functionally determined by zip, but zip
-- is not a key here. This is the BCNF violation the audit tier catches.
CREATE TABLE shipments (
  id       INTEGER PRIMARY KEY,
  order_id INTEGER NOT NULL REFERENCES orders(id),
  street   TEXT NOT NULL,
  zip      TEXT NOT NULL,
  city     TEXT NOT NULL,
  state    TEXT NOT NULL
);
