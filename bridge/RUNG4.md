# Rung 4 — migration-breaks-a-query as a *type error*

The extension ladder (see `../docs/SYNTHESIS.md`) climbs from "notothenia
proves schema properties with Alloy" toward "the compiler enforces the
schema on the code that uses it." Rung 4 is the migration-safety rung:

> Dropping a column from the schema should turn every query that depended
> on that column into a **compile error at its own call site** — before
> the build is green, before anything runs, without a database in the
> loop.

This is the strongest enforcer in the spectrum from
`../docs/notothenia-vs-db-enforcement.md`: not the database checking rows
at write time, not Alloy checking instances within a scope, but the
*compiler* checking every code path that names the schema. The cost is
that it only sees columns a query statically names — projection and
predicate reach — not arbitrary runtime SQL strings.

## The mechanism (no new type-level code)

yoga-postgres already resolves every column a query names against the
table type. `select @"name, email"` elaborates a `ParseSelect` /
`ResolveColumn` constraint per column; an unresolved column has no
instance, so the build stops with `NoInstanceFound`. Rung 4 is just this
mechanism pointed at a *schema migration*:

- **`src/Migration/SchemaV1.purs`** — the "before" schema, as notothenia's
  rung-1 codegen (`MinardDB.Codegen.YogaTable.emitModule`) emits it. Has
  an `email` column.
- **`src/Migration/SchemaV2.purs`** — the "after" schema, regenerated from
  a DDL that dropped `email`.
- **`src/Migration/Queries.purs`** — a manifest of typed queries. Compiles
  green, and that green build is itself a proof:
  - `userContactsV1` reaches `email`, type-checks against V1.
  - `userAgesV1` reaches only `name`/`age`, type-checks against V1.
  - `userAgesV2` — the *same* query — still type-checks against **V2**.
    The migration didn't break it, because it never reached the dropped
    column. V2 is a valid, queryable schema.

The other half can't sit in a green build by construction, so it lives in
the compile-fail harness:

- **`compile-fail-tests/MigrationBreaksQuery.purs`** — `userContacts`
  (reaching `email`) against **V2**. Must fail. First line declares the
  expected error substring, yoga's own convention:
  `-- EXPECT: NoInstanceFound`.
- **`compile-fail-tests/run.sh`** — drops each file into `src/`, builds
  `minard-bridge`, asserts the build failed carrying the EXPECT string,
  cleans up. A file here that *compiles* is a regression.

```
$ bash bridge/compile-fail-tests/run.sh
PASS MigrationBreaksQuery — failed with expected: NoInstanceFound
1 passed, 0 failed out of 1 compile-fail tests
```

## What the failure looks like

The error is call-site-precise — it names the offending column, the exact
`select` that reached it, and the query it belongs to:

```
[ERROR 1/1 NoInstanceFound] bridge/src/_CompileFailTest.purs:29:37

  29  userContacts = from V2.usersTable # select @"name, email" # where_ @"id = $id"
                                          ^^^^^^^^^^^^^^^^^^^^^

  Custom error:
    Column "email" not found in any table
  ...
  in value declaration userContacts
```

That is the whole rung-4 claim in one diagnostic: perform the migration,
and the compiler hands you the list of queries you must fix, each pinned
to where it breaks. The control (`userAgesV2`) stays green, so the signal
is specific to the dropped column — not noise from V2 being malformed.

## Honesty about scope

This is **projection-and-predicate reach** via yoga's resolver: the
columns a query names in `select`/`where`. It is *not* full dead-column
analysis (rung 3) — that needs yoga's parser classes to *emit* the set of
referenced columns so notothenia can diff it against the schema, a
separate upstream change tracked with the yoga maintainer. Rung 4 stands
on what yoga already enforces; rung 3 extends what yoga reports.

## Where this sits

`minard-db` itself stays yoga-dependency-free. Everything here depends on
yoga-postgres and lives only in `bridge/`, consumed from the harmonized
GitHub build (deps PR #1, merged 2026-05-30) pinned in the root
`spago.yaml`. Rung 4 is the first ladder rung with both halves built
continuously: the green manifest on every `spago build`, the red half on
every `run.sh`.
