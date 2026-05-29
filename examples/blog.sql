-- A small schema with real intent the DDL doesn't capture.
CREATE TABLE authors (
  id   INTEGER PRIMARY KEY,
  name TEXT NOT NULL
);

CREATE TABLE posts (
  id        INTEGER PRIMARY KEY,
  author_id INTEGER NOT NULL REFERENCES authors(id),
  parent_id INTEGER,            -- threaded replies; FK left undeclared by a legacy migration
  title     TEXT NOT NULL,
  body      TEXT NOT NULL
);
