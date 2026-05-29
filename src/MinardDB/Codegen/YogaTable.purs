-- | Codegen: notothenia `Schema` → rowtype-yoga type-level `Table`
-- | declarations (the forward half of the Schema-AST ⇄ typed-bindings
-- | bridge; see `docs/SYNTHESIS.md`, extension ladder rung 1).
-- |
-- | A notothenia `Schema` is a runtime *value* (parsed from DDL,
-- | introspected from a catalog) that we feed to Alloy. yoga's `Table`
-- | is a *type* the compiler checks queries against. Both encode the
-- | same object; this module emits the latter from the former, so a
-- | proven schema can become compile-time-checked query bindings.
-- |
-- | This module is intentionally **dependency-free** — it produces text.
-- | yoga is the *consuming* project's dependency, not notothenia's.
-- |
-- | Fidelity caveats (the bridge surfaces real gaps in our `Schema`):
-- |   * notothenia's `Column.defaultExpr` is a bare `Maybe String` that
-- |     has already lost the quote marks, so we can't reliably tell a
-- |     string *literal* default (`'idea'`) from a bareword/expression
-- |     default (`current_timestamp`). We heuristically classify:
-- |     contains `nextval` → AutoIncrement; is a known SQL expression or
-- |     contains `(` → `DefaultExpr`; otherwise → `Default` (literal).
-- |   * Only single-column UNIQUE constraints map to a per-column
-- |     `Unique` wrapper; composite uniques are dropped (noted).
-- |   * `PGBigInt`→`Int` and `PGDecimal`→`Number` are lossy.
module MinardDB.Codegen.YogaTable
  ( emitTable
  , emitModule
  , pgToPs
  , usedBaseImports
  ) where

import Prelude

import Data.Array as Array
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits as SCU
import MinardDB.Schema (Column, FKAction, ForeignKey, PGType(..), Schema, Table)

------------------------------------------------------------------------
-- Base type mapping
------------------------------------------------------------------------

-- | notothenia column type → the PureScript type yoga uses for it.
pgToPs :: PGType -> String
pgToPs = case _ of
  PGInt -> "Int"
  PGBigInt -> "Int"          -- lossy: no 64-bit Int in PS by default
  PGText -> "String"
  PGVarchar _ -> "String"
  PGBoolean -> "Boolean"
  PGTimestamp -> "DateTime"
  PGDate -> "PGDate"
  PGUUID -> "PGUUID"
  PGJsonb -> "Jsonb"
  PGDecimal -> "Number"      -- lossy: DECIMAL precision not preserved
  PGBlob -> "Foreign"

-- | The non-Prelude base types a schema's columns pull in, so a module
-- | emitter can import exactly what it uses.
usedBaseImports :: Schema -> Array { module_ :: String, name :: String }
usedBaseImports schema =
  Array.nubByEq (\a b -> a.name == b.name)
    (Array.mapMaybe baseImport allTypes)
  where
  allTypes = Array.concatMap (\t -> map _.dataType t.columns) schema.tables

  baseImport = case _ of
    PGTimestamp -> Just { module_: "Data.DateTime", name: "DateTime" }
    PGDate -> Just { module_: "Yoga.Postgres.Schema", name: "PGDate" }
    PGUUID -> Just { module_: "Yoga.Postgres.Schema", name: "PGUUID" }
    PGJsonb -> Just { module_: "Yoga.Postgres.Schema", name: "Jsonb" }
    PGBlob -> Just { module_: "Foreign", name: "Foreign" }
    _ -> Nothing

------------------------------------------------------------------------
-- Column wrapper composition
------------------------------------------------------------------------

-- | Default classification recovered (heuristically) from the lossy
-- | `defaultExpr :: Maybe String`.
data DefaultKind
  = NoDefault
  | AutoInc                 -- nextval(...) — the serial idiom
  | DefaultExprK String     -- an SQL expression default
  | DefaultLitK String      -- a literal default

classifyDefault :: Maybe String -> DefaultKind
classifyDefault = case _ of
  Nothing -> NoDefault
  Just e ->
    let le = String.toLower (String.trim e)
    in
      if String.contains (Pattern "nextval") le then AutoInc
      else if isExpr le then DefaultExprK e
      else DefaultLitK e
  where
  -- An expression default is a function call (`foo(...)`) or a known
  -- SQL niladic function. Boolean/`null` literals are NOT expressions —
  -- they map to a literal `Default`, matching yoga's own convention
  -- (`active :: Default "true" Boolean`).
  isExpr s =
    String.contains (Pattern "(") s
      || Array.elem s
          [ "current_timestamp", "now", "current_date", "current_time" ]

isNoDefault :: DefaultKind -> Boolean
isNoDefault NoDefault = true
isNoDefault _ = false

isAutoInc :: DefaultKind -> Boolean
isAutoInc AutoInc = true
isAutoInc _ = false

isDefaultLit :: DefaultKind -> Boolean
isDefaultLit (DefaultLitK _) = true
isDefaultLit _ = false

isDefaultExpr :: DefaultKind -> Boolean
isDefaultExpr (DefaultExprK _) = true
isDefaultExpr _ = false

-- | Compose the wrapper stack for one column, outermost first:
-- | PrimaryKey ▸ AutoIncrement ▸ ForeignKey ▸ Unique ▸ Default* ▸
-- | Nullable ▸ base. Only the wrappers that apply are emitted.
columnType :: Table -> Column -> String
columnType t col =
  let
    base = pgToPs col.dataType
    isPK = Array.elem col.name t.primaryKey
    dk = classifyDefault col.defaultExpr
    isUnique = Array.any (\uc -> uc.columns == [ col.name ]) t.uniqueConstraints
    mFK = Array.find (\fk -> fk.columns == [ col.name ]) t.foreignKeys
    -- A PK column is never emitted as Nullable; a column with a default
    -- is effectively non-null on insert, so we don't mark it Nullable.
    nullable = col.nullable && not isPK && isNoDefault dk

    -- innermost → outermost, applied by `wrap`
    wrappers =
      prepend (isPK) "PrimaryKey"
        $ prepend (isAutoInc dk) "AutoIncrement"
        $ prependMaybe (map fkWrapper mFK)
        $ prepend isUnique "Unique"
        $ prependDefault dk
        $ prepend nullable "Nullable"
        $ []
  in
    wrap wrappers base
  where
  prepend cond w rest = if cond then [ w ] <> rest else rest
  prependMaybe = case _ of
    Just w -> \rest -> [ w ] <> rest
    Nothing -> \rest -> rest
  prependDefault = case _ of
    DefaultLitK v -> \rest -> [ "Default " <> show v ] <> rest
    DefaultExprK e -> \rest -> [ "DefaultExpr " <> show e ] <> rest
    _ -> \rest -> rest

  fkWrapper :: ForeignKey -> String
  fkWrapper fk =
    "ForeignKey " <> show fk.refTable <> " References "
      <> show (firstCol fk) <> fkActionNote fk.onDelete

-- | Wrap a base type with a stack of prefixes (outermost first).
-- | A single-token inner is left unparenthesised for readability.
wrap :: Array String -> String -> String
wrap wrappers base = Array.foldr step base wrappers
  where
  step w acc =
    if isSingleToken acc then w <> " " <> acc
    else w <> " (" <> acc <> ")"

isSingleToken :: String -> Boolean
isSingleToken s = not (String.contains (Pattern " ") s)

firstCol :: ForeignKey -> String
firstCol fk = case Array.head fk.refColumns of
  Just c -> c
  Nothing -> ""

-- ON DELETE actions aren't carried in yoga's ForeignKey type; we drop
-- them silently here (they belong to Alloy's migration model, not the
-- query-conformance type). Kept as a hook in case that changes.
fkActionNote :: FKAction -> String
fkActionNote _ = ""

------------------------------------------------------------------------
-- Table + module emission
------------------------------------------------------------------------

-- | Emit `type FooTable = Table "foo" ( ... )` for one table.
emitTable :: Table -> String
emitTable t =
  "type " <> pascal t.name <> "Table = Table " <> show t.name <> "\n"
    <> "  ( " <> String.joinWith "\n  , " (map column t.columns) <> "\n  )"
  where
  column col = col.name <> " :: " <> columnType t col

-- | Emit a complete, compilable module containing every table's
-- | declaration, importing exactly the constructors used.
emitModule :: String -> Schema -> String
emitModule moduleName schema =
  String.joinWith "\n"
    [ "-- | Generated by MinardDB.Codegen.YogaTable from the "
        <> show schema.name <> " schema. Do not edit by hand."
    , "module " <> moduleName <> " where"
    , ""
    , "import Yoga.Postgres.Schema (Table, " <> String.joinWith ", " schemaCtors <> ")"
    , baseImportLines
    , ""
    , String.joinWith "\n\n" (map emitTable schema.tables)
    , ""
    ]
  where
  -- Constructors from Yoga.Postgres.Schema we might reference. We import
  -- the full set used across the schema; unused ones would warn, so we
  -- compute the actual set.
  schemaCtors =
    Array.nub $ Array.concatMap tableCtors schema.tables

  tableCtors t = Array.concatMap (colCtors t) t.columns

  colCtors t col =
    let dk = classifyDefault col.defaultExpr
    in
      (if Array.elem col.name t.primaryKey then [ "PrimaryKey" ] else [])
        <> (if isAutoInc dk then [ "AutoIncrement" ] else [])
        <> (if Array.any (\uc -> uc.columns == [ col.name ]) t.uniqueConstraints then [ "Unique" ] else [])
        <> (if isDefaultLit dk then [ "Default" ] else [])
        <> (if isDefaultExpr dk then [ "DefaultExpr" ] else [])
        <> (if needsNullable t col then [ "Nullable" ] else [])
        <> (if Array.any (\fk -> fk.columns == [ col.name ]) t.foreignKeys then [ "ForeignKey", "References" ] else [])

  needsNullable t col =
    let dk = classifyDefault col.defaultExpr
    in col.nullable && not (Array.elem col.name t.primaryKey) && isNoDefault dk

  baseImportLines =
    String.joinWith "\n"
      (map (\i -> "import " <> i.module_ <> " (" <> i.name <> ")") (usedBaseImports schema))

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

-- | snake_case / lower → PascalCase (projects → Projects,
-- | status_history → StatusHistory).
pascal :: String -> String
pascal s =
  String.joinWith ""
    (map upperFirst (String.split (Pattern "_") s))

upperFirst :: String -> String
upperFirst s = case SCU.uncons s of
  Just { head, tail } -> String.toUpper (SCU.singleton head) <> tail
  Nothing -> s
