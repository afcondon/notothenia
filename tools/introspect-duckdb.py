#!/usr/bin/env python3
"""Introspect a DuckDB file's main schema into a Schema-AST-shaped JSON.

Usage:
    python3 introspect-duckdb.py /path/to/file.duckdb [output.json]

Reads the database in read-only mode. Output JSON matches the shape
consumed by MinardDB.Schema.* via MinardDB.Schema.JSON.

The DuckDB CLI must be on PATH.
"""

import json
import subprocess
import sys
from pathlib import Path


def duckdb_json(db_path: str, sql: str) -> list:
    """Run a query and parse the JSON output. Read-only mode."""
    result = subprocess.run(
        ["duckdb", "-readonly", "-json", db_path, sql],
        capture_output=True, text=True, check=False,
    )
    if result.returncode != 0:
        raise RuntimeError(f"duckdb failed: {result.stderr}")
    if not result.stdout.strip():
        return []
    return json.loads(result.stdout)


DUCKDB_TO_PG_TYPE = {
    "INTEGER": "PGInt",
    "BIGINT": "PGBigInt",
    "VARCHAR": "PGText",
    "TEXT": "PGText",
    "BOOLEAN": "PGBoolean",
    "TIMESTAMP": "PGTimestamp",
    "TIMESTAMP WITH TIME ZONE": "PGTimestamp",
    "DATE": "PGDate",
    "UUID": "PGUUID",
    "JSON": "PGJsonb",
    "JSONB": "PGJsonb",
}


def map_type(data_type: str) -> str:
    """Map DuckDB data_type string to our PGType ADT."""
    base = data_type.upper().split("(")[0].strip()
    return DUCKDB_TO_PG_TYPE.get(base, "PGText")  # default to PGText for now


def introspect(db_path: str) -> dict:
    # ---- tables ----
    tables = duckdb_json(db_path, """
        SELECT table_name, table_schema
        FROM information_schema.tables
        WHERE table_schema = 'main'
        ORDER BY table_name
    """)

    # ---- columns (one query, group in python) ----
    columns_raw = duckdb_json(db_path, """
        SELECT table_name, column_name, data_type, is_nullable, column_default, ordinal_position
        FROM information_schema.columns
        WHERE table_schema = 'main'
        ORDER BY table_name, ordinal_position
    """)
    columns_by_table = {}
    for c in columns_raw:
        columns_by_table.setdefault(c["table_name"], []).append({
            "name": c["column_name"],
            "dataType": map_type(c["data_type"]),
            "nullable": c["is_nullable"] in ("YES", True, "true"),
            "defaultExpr": c["column_default"],
        })

    # ---- primary keys ----
    pk_raw = duckdb_json(db_path, """
        SELECT
            tc.table_name,
            kcu.column_name,
            kcu.ordinal_position
        FROM information_schema.table_constraints tc
        JOIN information_schema.key_column_usage kcu
          ON tc.constraint_name = kcu.constraint_name
        WHERE tc.table_schema = 'main'
          AND tc.constraint_type = 'PRIMARY KEY'
        ORDER BY tc.table_name, kcu.ordinal_position
    """)
    pks_by_table = {}
    for r in pk_raw:
        pks_by_table.setdefault(r["table_name"], []).append(r["column_name"])

    # ---- unique constraints ----
    uq_raw = duckdb_json(db_path, """
        SELECT
            tc.table_name,
            tc.constraint_name,
            kcu.column_name,
            kcu.ordinal_position
        FROM information_schema.table_constraints tc
        JOIN information_schema.key_column_usage kcu
          ON tc.constraint_name = kcu.constraint_name
        WHERE tc.table_schema = 'main'
          AND tc.constraint_type = 'UNIQUE'
        ORDER BY tc.table_name, tc.constraint_name, kcu.ordinal_position
    """)
    uqs_by_table = {}
    for r in uq_raw:
        key = (r["table_name"], r["constraint_name"])
        uqs_by_table.setdefault(key, []).append(r["column_name"])
    uniques_by_table = {}
    for (tname, _cname), cols in uqs_by_table.items():
        uniques_by_table.setdefault(tname, []).append({"columns": cols})

    # ---- foreign keys ----
    # Join referential_constraints -> key_column_usage to get FK cols
    # Join referential_constraints -> table_constraints (via unique_constraint_name)
    #   then -> key_column_usage to get referenced cols + ref table
    fk_raw = duckdb_json(db_path, """
        SELECT
            rc.constraint_name        AS fk_name,
            kcu.table_name            AS fk_table,
            kcu.column_name           AS fk_column,
            kcu.ordinal_position      AS fk_pos,
            rc.unique_constraint_name AS ref_constraint,
            rc.update_rule,
            rc.delete_rule
        FROM information_schema.referential_constraints rc
        JOIN information_schema.key_column_usage kcu
          ON rc.constraint_name = kcu.constraint_name
        WHERE rc.constraint_schema = 'main'
        ORDER BY rc.constraint_name, kcu.ordinal_position
    """)
    ref_kcu_raw = duckdb_json(db_path, """
        SELECT
            constraint_name,
            table_name        AS ref_table,
            column_name       AS ref_column,
            ordinal_position
        FROM information_schema.key_column_usage
        WHERE constraint_schema = 'main'
        ORDER BY constraint_name, ordinal_position
    """)
    ref_cols_by_constraint = {}
    ref_table_by_constraint = {}
    for r in ref_kcu_raw:
        ref_cols_by_constraint.setdefault(r["constraint_name"], []).append(r["ref_column"])
        ref_table_by_constraint[r["constraint_name"]] = r["ref_table"]

    fks_by_table = {}
    fk_buckets = {}
    for r in fk_raw:
        fk_buckets.setdefault(r["fk_name"], {
            "fk_table": r["fk_table"],
            "columns": [],
            "ref_constraint": r["ref_constraint"],
            "update_rule": r["update_rule"],
            "delete_rule": r["delete_rule"],
        })
        fk_buckets[r["fk_name"]]["columns"].append(r["fk_column"])

    for fk_name, info in fk_buckets.items():
        ref_table = ref_table_by_constraint.get(info["ref_constraint"], "?")
        ref_cols = ref_cols_by_constraint.get(info["ref_constraint"], [])
        fks_by_table.setdefault(info["fk_table"], []).append({
            "columns": info["columns"],
            "refTable": ref_table,
            "refColumns": ref_cols,
            "onDelete": fk_action(info["delete_rule"]),
            "onUpdate": fk_action(info["update_rule"]),
        })

    # ---- inferred FKs (by `<entity>_id` naming convention) ----
    table_names = {t["table_name"] for t in tables}
    inferred_by_table = {}
    for tname, cols in columns_by_table.items():
        declared_fk_cols = {
            c for fk in fks_by_table.get(tname, [])
            for c in fk["columns"]
        }
        for col in cols:
            cname = col["name"]
            if cname == "id" or not cname.endswith("_id"):
                continue
            if cname in declared_fk_cols:
                continue
            target = infer_target_table(cname, table_names, own_table=tname)
            if target is None:
                continue
            inferred_by_table.setdefault(tname, []).append({
                "columns": [cname],
                "refTable": target,
                "refColumns": ["id"],
                "onDelete": "NoAction",
                "onUpdate": "NoAction",
                "source": "inferred",
            })

    # ---- assemble ----
    schema = {
        "name": Path(db_path).stem,
        "tables": [
            {
                "name": t["table_name"],
                "schemaName": t["table_schema"],
                "columns": columns_by_table.get(t["table_name"], []),
                "primaryKey": pks_by_table.get(t["table_name"], []),
                "foreignKeys": fks_by_table.get(t["table_name"], []),
                "inferredForeignKeys": inferred_by_table.get(t["table_name"], []),
                "uniqueConstraints": uniques_by_table.get(t["table_name"], []),
            }
            for t in tables
        ],
    }
    return schema


def infer_target_table(col_name: str, table_names: set, own_table: str):
    """Given a column like `project_id`, find a plausible target table.

    Strategies, in priority order:
      1. `<stem>s`  — singular noun → plural table name (project_id → projects)
      2. `<stem>`   — column stem matches table name directly (tag_id → tags? no)
      3. Self-reference: parent_id, parent_<x>_id → own_table
      4. Common pattern: blocker_id, blocked_id, source_id, target_id, supersedes_id
         when own_table is plausibly a relationship table or related concept
    """
    stem = col_name[:-3]  # strip "_id"
    # Strategy 1: pluralize
    if (stem + "s") in table_names:
        return stem + "s"
    # Strategy 2: stem matches directly (e.g. "metadata_id" -> "metadata")
    if stem in table_names:
        return stem
    # Strategy 3: parent_id and self-reference patterns
    if stem in ("parent", "supersedes"):
        return own_table
    # Strategy 4: "blocker"/"blocked"/"source"/"target" often reference projects
    # for project-relationship tables; check if "projects" exists as a guess
    if stem in ("blocker", "blocked", "source", "target") and "projects" in table_names:
        return "projects"
    return None


def fk_action(rule: str) -> str:
    """Map SQL action string to our FKAction ADT."""
    if not rule:
        return "NoAction"
    r = rule.upper().replace(" ", "").replace("-", "")
    return {
        "CASCADE": "Cascade",
        "SETNULL": "SetNull",
        "RESTRICT": "Restrict",
        "NOACTION": "NoAction",
    }.get(r, "NoAction")


def main():
    if len(sys.argv) < 2:
        print(__doc__.strip())
        sys.exit(1)
    db_path = sys.argv[1]
    out_path = sys.argv[2] if len(sys.argv) > 2 else None
    schema = introspect(db_path)
    blob = json.dumps(schema, indent=2)
    if out_path:
        Path(out_path).write_text(blob)
        print(f"wrote {out_path}: {len(schema['tables'])} tables", file=sys.stderr)
    else:
        print(blob)


if __name__ == "__main__":
    main()
