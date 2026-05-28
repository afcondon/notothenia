# Minard-DB

Schema cartography with Alloy-backed proofs of correctness.

Part of the [Hylograph tool ecosystem](../portolan/ECOSYSTEM.md) alongside
Minard-PS and Portolan.

## What it does

Database schemas are type systems. Normal forms are type safety levels,
foreign keys are type-level references, denormalization is `unsafeCoerce`,
migrations are type-system evolution. Minard-DB makes this concrete:

1. **Ingest** a schema (DDL parsing or catalog introspection) into a
   typed PureScript AST.
2. **Generate** an Alloy model from the AST — each table becomes a
   `sig`, each FK becomes a relational field, each constraint a `fact`.
3. **Verify** properties (BCNF, lossless decomposition, migration safety,
   FK acyclicity) by running Alloy's SAT solver. Get a proof within
   scope, or a concrete counterexample.
4. **Visualize** (planned) — Hylograph topology with normal-form
   coloring, query reach maps, migration timelines.

## Prerequisites

```bash
# Java (Alloy is a JVM tool)
brew install openjdk
sudo ln -sfn /opt/homebrew/opt/openjdk/libexec/openjdk.jdk \
             /Library/Java/JavaVirtualMachines/openjdk.jdk

# Alloy jar (vendored, gitignored)
mkdir -p vendor && curl -L \
  -o vendor/alloy.jar \
  'https://github.com/AlloyTools/org.alloytools.alloy/releases/download/v6.2.0/org.alloytools.alloy.dist.jar'

# PureScript deps
spago build
```

## Smoke test

```bash
spago run -p minard-db --main MinardDB.Smoke
# Generates /tmp/minard-smoke.als from a hand-written Schema AST

java -jar vendor/alloy.jar exec /tmp/minard-smoke.als
# Runs Alloy, produces minard-smoke/receipt.json + solution markdowns
```

## Status

Phase 1 in progress. End-to-end working: PureScript Schema AST →
generated Alloy model → SAT solver result. Next: PureScript-side
invocation of the Alloy CLI + receipt.json parsing.

See `/Users/afc/.claude/plans/idempotent-whistling-crayon.md` for the
full design sketch.
