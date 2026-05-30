#!/bin/bash
# Rung-4 compile-fail harness for minard-bridge.
#
# Mirrors yoga-postgres/compile-fail-tests/run.sh: every .purs in this
# directory MUST fail to compile, and its first line declares the error
# substring it must fail with:
#
#     -- EXPECT: NoInstanceFound
#
# For each file the harness rewrites its module header, drops it into
# bridge/src/ so it builds as part of minard-bridge (against the real
# schema modules it imports), runs `spago build`, removes it, and checks
# that the build failed carrying the EXPECT substring. A file that
# compiles is itself a failure — the type-level check it documents has
# regressed.
set -u
cd "$(dirname "$0")/../.." || exit 1   # -> minard-db workspace root

SRC="bridge/src/_CompileFailTest.purs"
trap 'rm -f "$SRC"' EXIT

PASS=0
FAIL=0
TOTAL=0

for f in bridge/compile-fail-tests/*.purs; do
  EXPECT=$(head -1 "$f" | sed 's/-- EXPECT: //')
  NAME=$(basename "$f" .purs)
  TOTAL=$((TOTAL + 1))

  sed "s/^module .*/module Minard.Bridge.CompileFailTest where/" "$f" > "$SRC"
  OUTPUT=$(spago build -p minard-bridge 2>&1) || true
  rm -f "$SRC"

  if echo "$OUTPUT" | grep -q "Build succeeded"; then
    echo "FAIL $NAME — compiled successfully (should have failed)"
    FAIL=$((FAIL + 1))
  elif echo "$OUTPUT" | grep -q "$EXPECT"; then
    echo "PASS $NAME — failed with expected: $EXPECT"
    PASS=$((PASS + 1))
  else
    echo "FAIL $NAME — failed but without expected '$EXPECT'"
    echo "     Got: $(echo "$OUTPUT" | grep -i 'error' | head -3)"
    FAIL=$((FAIL + 1))
  fi
done

echo ""
echo "$PASS passed, $FAIL failed out of $TOTAL compile-fail tests"

[ "$FAIL" -ne 0 ] && exit 1
exit 0
