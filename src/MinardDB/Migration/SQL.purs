-- | Parse a SQL migration script into a `MigrationSequence`.
-- |
-- | This is intentionally a small subset of DDL — just enough to round-
-- | trip migrations through the same pipeline as the hand-coded
-- | `Migration.Smoke` fixtures. The shape we accept:
-- |
-- |   CREATE TABLE [IF NOT EXISTS] [schema.]name (
-- |     col_def,
-- |     col_def,
-- |     [CONSTRAINT name] PRIMARY KEY (cols),
-- |     [CONSTRAINT name] FOREIGN KEY (cols) REFERENCES tbl(cols)
-- |                       [ON DELETE action] [ON UPDATE action],
-- |     [CONSTRAINT name] UNIQUE (cols)
-- |   );
-- |
-- |   col_def = name TYPE [NOT NULL] [PRIMARY KEY] [UNIQUE]
-- |                       [DEFAULT expr] [REFERENCES tbl(cols)]
-- |
-- |   DROP TABLE [IF EXISTS] name;
-- |
-- |   ALTER TABLE name ADD [COLUMN] col_def;
-- |   ALTER TABLE name DROP COLUMN [IF EXISTS] name;
-- |   ALTER TABLE name ADD [CONSTRAINT n] FOREIGN KEY (cols)
-- |     REFERENCES tbl(cols) [ON DELETE a] [ON UPDATE a];
-- |   ALTER TABLE name DROP FOREIGN KEY (cols);
-- |
-- | `--` line comments and `/* … */` block comments are stripped.
-- |
-- | What we do NOT handle (deliberately): generated columns, CHECK
-- | constraints, indexes, views, triggers, CTE-based DDL, sequences,
-- | extensions, EXTENSION-IF-NOT-EXISTS, COMMENT ON, vendor-specific
-- | dialects. The proof pipeline doesn't need them; if a real schema
-- | dump exercises them, the parser will fail with a positional error
-- | the caller can rewrite around.
module MinardDB.Migration.SQL
  ( parseSql
  , dbmateUp
  , schemaFromSql
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.String (Pattern(..))
import Data.String as String
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (toLower)
import MinardDB.Migration (Migration(..), MigrationSequence, applyMigration)
import MinardDB.SQL.Lexer (identifierWith, integer, isAsciiDigit, keyword, keywords, lexeme, parens, rawWord, skipFiller, symbol)
import MinardDB.Schema (Column, FKAction(..), ForeignKey, PGType(..), Schema, Table)
import Parsing (Parser, fail, parseErrorMessage, runParser)
import Parsing.Combinators (choice, lookAhead, manyTill, option, optionMaybe, sepBy1, try)
import Parsing.String (anyChar, char, eof, satisfy)

-- | Top-level entry. Parses a multi-statement script; returns the
-- | sequence of migrations or the first parse error.
parseSql :: String -> Either String MigrationSequence
parseSql src = case runParser src topLevel of
  Left err -> Left (parseErrorMessage err)
  Right ms -> Right ms

-- | Parse a `.sql` schema dump into a `Schema` by parsing the DDL into
-- | a migration sequence (CREATE TABLE + ALTER TABLE ADD COLUMN; other
-- | statements are skipped by `topLevel`) and replaying it onto an
-- | empty schema. The empty schema's `name` is supplied by the caller.
-- |
-- | The replay is *tolerant*: a step that errors (a `CREATE TABLE IF
-- | NOT EXISTS` for a table already present, an `ADD COLUMN IF NOT
-- | EXISTS` for a column the CREATE already declared — both common in
-- | idempotent real-world dumps) is skipped rather than aborting. A
-- | schema dump is "ensure this exists" DDL, so every statement is
-- | effectively idempotent; strict sequencing is for migration
-- | *verification*, not schema ingestion.
-- |
-- | This is how reach analysis ingests a real schema: point it at the
-- | project's `schema.sql` and get back the `Schema` the queries are
-- | resolved against.
schemaFromSql :: String -> String -> Either String Schema
schemaFromSql name src = do
  ms <- parseSql src
  pure (Array.foldl applyTolerant { name, tables: [] } ms)
  where
  applyTolerant schema m = case applyMigration m schema of
    Left _ -> schema
    Right next -> next

-- | Extract the forward (`up`) section from a dbmate-style migration
-- | file. dbmate (and several other migration tools) put both the
-- | forward and rollback DDL in one file, delimited by magic line
-- | comments:
-- |
-- |     -- migrate:up
-- |     CREATE TABLE … ;
-- |     -- migrate:down
-- |     DROP TABLE … ;
-- |
-- | Because those delimiters are `--` comments, the parser's own
-- | comment-stripping would eat the markers and then happily parse the
-- | rollback DDL as more forward migrations. So we slice the up-section
-- | out *before* parsing: keep everything from `-- migrate:up` (if
-- | present) up to `-- migrate:down` (if present). A plain `.sql` file
-- | with neither marker passes through unchanged.
dbmateUp :: String -> String
dbmateUp src =
  let
    afterUp = case String.indexOf (Pattern "-- migrate:up") src of
      Just i -> String.drop i src
      Nothing -> src
  in
    case String.indexOf (Pattern "-- migrate:down") afterUp of
      Just j -> String.take j afterUp
      Nothing -> afterUp

------------------------------------------------------------------------
-- DDL identifiers (token layer is shared in MinardDB.SQL.Lexer)
------------------------------------------------------------------------

-- | An identifier that isn't one of the DDL grammar's reserved words.
identifier :: Parser String String
identifier = identifierWith isReserved

-- | Reserved words the DDL grammar uses syntactically. Quoted
-- | identifiers bypass this list, so a table literally called `"order"`
-- | still parses.
-- | NB: `key` is deliberately NOT here — it's a common column name
-- | (`metadata.key`), and the `keyword` parser matches `PRIMARY KEY` /
-- | `FOREIGN KEY` without consulting this list, so reserving it only
-- | blocks legitimate identifiers.
isReserved :: String -> Boolean
isReserved w = Array.elem w
  [ "add", "alter", "cascade", "column", "constraint", "create"
  , "default", "delete", "drop", "exists", "foreign", "if"
  , "no", "not", "null", "on", "primary", "references", "restrict"
  , "set", "table", "unique", "update"
  ]

-- | A possibly-schema-qualified name; the schema part is dropped. The
-- | Schema AST tracks `schemaName` per table but the migration
-- | vocabulary identifies tables by `name` alone.
qualifiedName :: Parser String String
qualifiedName = do
  first <- identifier
  rest <- optionMaybe (try (symbol "." *> identifier))
  pure (fromMaybe first rest)

-- | One-or-more identifiers separated by commas, returned as an Array.
columnList :: Parser String (Array String)
columnList = do
  cs <- sepBy1 identifier (symbol ",")
  pure (Array.fromFoldable cs)

------------------------------------------------------------------------
-- Types and FK actions
------------------------------------------------------------------------

pgType :: Parser String PGType
pgType = choice $ map try
  [ keyword "integer" *> pure PGInt
  , keyword "int" *> pure PGInt
  , keyword "bigint" *> pure PGBigInt
  , keyword "text" *> pure PGText
  , do
      keyword "varchar"
      n <- option 255 (try (parens integer))
      pure (PGVarchar n)
  , keyword "boolean" *> pure PGBoolean
  , keyword "bool" *> pure PGBoolean
  , keyword "timestamp" *> pure PGTimestamp
  , keyword "date" *> pure PGDate
  , keyword "uuid" *> pure PGUUID
  , keyword "jsonb" *> pure PGJsonb
  , keyword "json" *> pure PGJsonb
  , do
      keyword "decimal" <|> keyword "numeric"
      -- optional (precision, scale) — consumed, not retained
      _ <- option [] (try (parens (sepBy1 integer (symbol ",") <#> Array.fromFoldable)))
      pure PGDecimal
  , keyword "blob" *> pure PGBlob
  , keyword "bytea" *> pure PGBlob
  ]

fkAction :: Parser String FKAction
fkAction = choice $ map try
  [ keyword "cascade" *> pure Cascade
  , keywords [ "set", "null" ] *> pure SetNull
  , keyword "restrict" *> pure Restrict
  , keywords [ "no", "action" ] *> pure NoAction
  ]

-- | The two trailing clauses on a REFERENCES specification. Limited to
-- | ON DELETE first, ON UPDATE second; if a real script uses the other
-- | order we'll cross that bridge when it breaks something.
referencesTail :: Parser String { onDelete :: FKAction, onUpdate :: FKAction }
referencesTail = do
  od <- option NoAction (try (keywords [ "on", "delete" ] *> fkAction))
  ou <- option NoAction (try (keywords [ "on", "update" ] *> fkAction))
  pure { onDelete: od, onUpdate: ou }

------------------------------------------------------------------------
-- CREATE TABLE body
------------------------------------------------------------------------

type RefSpec =
  { refTable :: String
  , refColumns :: Array String
  , onDelete :: FKAction
  , onUpdate :: FKAction
  }

-- | One element inside the parenthesised CREATE TABLE body. Either a
-- | column with its inline modifiers, or a table-level constraint.
data TableItem
  = TIColumn
      { col :: Column
      , isPK :: Boolean
      , inlineUnique :: Boolean
      , inlineFK :: Maybe RefSpec
      }
  | TIPrimaryKey (Array String)
  | TIForeignKey ForeignKey
  | TIUnique (Array String)

tableItem :: Parser String TableItem
tableItem =
      try namedConstraint
  <|> try constraintBody
  <|> columnDef
  where
  namedConstraint = do
    keyword "constraint"
    _ <- identifier
    constraintBody

constraintBody :: Parser String TableItem
constraintBody =
      try pkBody
  <|> try fkBody
  <|> uqBody
  where
  pkBody = do
    keywords [ "primary", "key" ]
    cols <- parens columnList
    pure (TIPrimaryKey cols)

  fkBody = do
    keywords [ "foreign", "key" ]
    cols <- parens columnList
    keyword "references"
    refTable <- qualifiedName
    refColumns <- option [] (try (parens columnList))
    tail <- referencesTail
    pure (TIForeignKey
      { columns: cols
      , refTable
      , refColumns
      , onDelete: tail.onDelete
      , onUpdate: tail.onUpdate
      })

  uqBody = do
    keyword "unique"
    cols <- parens columnList
    pure (TIUnique cols)

-- | A column definition: name, type, and any inline modifiers.
columnDef :: Parser String TableItem
columnDef = do
  name <- identifier
  ty <- pgType
  mods <- Array.many (try columnModifier)
  let
    isPK = Array.any isMPrimaryKey mods
    explicitNotNull = Array.any isMNotNull mods
    nullable = not isPK && not explicitNotNull
    defaultExpr = Array.findMap modDefault mods
    inlineFK = Array.findMap modRefs mods
    inlineUnique = Array.any isMUnique mods
    col = { name, dataType: ty, nullable, defaultExpr }
  pure (TIColumn { col, isPK, inlineUnique, inlineFK })

isMPrimaryKey :: ColModifier -> Boolean
isMPrimaryKey MPrimaryKey = true
isMPrimaryKey _ = false

isMNotNull :: ColModifier -> Boolean
isMNotNull MNotNull = true
isMNotNull _ = false

isMUnique :: ColModifier -> Boolean
isMUnique MUnique = true
isMUnique _ = false

modDefault :: ColModifier -> Maybe String
modDefault (MDefault e) = Just e
modDefault _ = Nothing

modRefs :: ColModifier -> Maybe RefSpec
modRefs (MRefs r) = Just r
modRefs _ = Nothing

data ColModifier
  = MNotNull
  | MNullable
  | MPrimaryKey
  | MUnique
  | MDefault String
  | MRefs RefSpec
  | MIgnored

columnModifier :: Parser String ColModifier
columnModifier = choice $ map try
  [ keywords [ "not", "null" ] *> pure MNotNull
  , keyword "null" *> pure MNullable
  , keywords [ "primary", "key" ] *> pure MPrimaryKey
  , keyword "unique" *> pure MUnique
  -- No-op-for-our-purposes column constraints. We accept and discard
  -- them so real DDL parses; they carry no information the RI model
  -- consumes. AUTOINCREMENT / AUTO_INCREMENT is the common one in the
  -- wild (SQLite, MySQL).
  , keyword "autoincrement" *> pure MIgnored
  , keyword "auto_increment" *> pure MIgnored
  , do
      keyword "default"
      e <- defaultLiteral
      pure (MDefault e)
  , do
      keyword "references"
      refTable <- qualifiedName
      -- The referenced column list is optional in SQL — omitted means
      -- "the referenced table's primary key". We don't resolve that
      -- here (it'd need the target table's definition, which may not be
      -- parsed yet), so an omitted list leaves refColumns empty and the
      -- FK is checked at table granularity only.
      refColumns <- option [] (try (parens columnList))
      tail <- referencesTail
      pure (MRefs
        { refTable
        , refColumns
        , onDelete: tail.onDelete
        , onUpdate: tail.onUpdate
        })
  ]

-- | A token of a default expression: string literal, numeric literal,
-- | or a bareword optionally followed by a call argument list
-- | (`current_timestamp`, `true`, `nextval('seq_projects')`). We store
-- | the surface text; downstream we don't interpret it — we only need to
-- | *consume* it so the column definition parses.
defaultLiteral :: Parser String String
defaultLiteral = lexeme $ choice $ map try
  [ do
      _ <- char '\''
      cs <- Array.many (satisfy (_ /= '\''))
      _ <- char '\''
      pure (fromCharArray cs)
  , do
      ds <- Array.some (try (satisfy isAsciiDigit))
      pure (fromCharArray ds)
  , do
      w <- rawWord
      -- Optional call args, e.g. nextval('seq_projects'). One level of
      -- parens, contents opaque — enough for the function-call defaults
      -- that appear in real schema dumps.
      args <- option "" parenChunk
      pure (w <> args)
  ]
  where
  parenChunk = do
    _ <- char '('
    inner <- Array.many (satisfy (_ /= ')'))
    _ <- char ')'
    pure ("(" <> fromCharArray inner <> ")")

------------------------------------------------------------------------
-- Statement-level
------------------------------------------------------------------------

topLevel :: Parser String MigrationSequence
topLevel = do
  skipFiller
  ms <- gather []
  eof
  pure ms
  where
  gather acc = do
    atEnd <- (eof *> pure true) <|> pure false
    if atEnd then pure acc
    else do
      -- Try a statement we model; if it doesn't parse (CREATE
      -- SEQUENCE / INDEX / VIEW, INSERT, PRAGMA, or any ALTER form we
      -- don't handle), skip to the next `;` and carry on. This lets a
      -- real schema dump through — we ingest the table structure and
      -- silently drop the rest, rather than failing the whole parse.
      mStmt <- optionMaybe (try (statement <* symbol ";"))
      case mStmt of
        Just m -> gather (Array.snoc acc m)
        Nothing -> do
          skipStatement
          gather acc

  -- Consume everything up to and including the next `;` (or to eof).
  skipStatement = do
    _ <- manyTill anyChar (void (char ';') <|> eof)
    skipFiller

-- | Dispatch on the leading keyword. We `lookAhead` to peek without
-- | committing; the chosen branch re-parses the keyword for itself.
statement :: Parser String Migration
statement = do
  kw <- lookAhead (lexeme (toLower <$> rawWord))
  case kw of
    "create" -> createTable
    "drop" -> dropTable
    "alter" -> alterTable
    other -> fail ("unexpected statement leader `" <> other <> "`")

createTable :: Parser String Migration
createTable = do
  keyword "create"
  keyword "table"
  _ <- optionMaybe (keywords [ "if", "not", "exists" ])
  name <- qualifiedName
  items <- parens do
    xs <- sepBy1 tableItem (symbol ",")
    pure (Array.fromFoldable xs)
  pure (CreateTable (assembleTable name items))

assembleTable :: String -> Array TableItem -> Table
assembleTable name items =
  let
    columns = Array.mapMaybe itemColumn items
    inlinePKs = Array.mapMaybe itemColumnPKName items
    tablePKs = Array.mapMaybe itemTablePK items
    primaryKey = case Array.head tablePKs of
      Just cs -> cs
      Nothing -> inlinePKs
    inlineFKs = Array.mapMaybe (itemInlineFK) items
    tableFKs = Array.mapMaybe itemTableFK items
    inlineUniques = Array.mapMaybe itemInlineUnique items
    tableUniques = Array.mapMaybe itemTableUnique items
  in
    { name
    , schemaName: "main"
    , columns
    , primaryKey
    , foreignKeys: inlineFKs <> tableFKs
    , uniqueConstraints: inlineUniques <> tableUniques
    , functionalDependencies: []
    }

itemColumn :: TableItem -> Maybe Column
itemColumn (TIColumn r) = Just r.col
itemColumn _ = Nothing

itemColumnPKName :: TableItem -> Maybe String
itemColumnPKName (TIColumn r) = if r.isPK then Just r.col.name else Nothing
itemColumnPKName _ = Nothing

itemTablePK :: TableItem -> Maybe (Array String)
itemTablePK (TIPrimaryKey cs) = Just cs
itemTablePK _ = Nothing

itemInlineFK :: TableItem -> Maybe ForeignKey
itemInlineFK (TIColumn r) = r.inlineFK <#> \ref ->
  { columns: [ r.col.name ]
  , refTable: ref.refTable
  , refColumns: ref.refColumns
  , onDelete: ref.onDelete
  , onUpdate: ref.onUpdate
  }
itemInlineFK _ = Nothing

itemTableFK :: TableItem -> Maybe ForeignKey
itemTableFK (TIForeignKey fk) = Just fk
itemTableFK _ = Nothing

itemInlineUnique :: TableItem -> Maybe { columns :: Array String }
itemInlineUnique (TIColumn r) =
  if r.inlineUnique then Just { columns: [ r.col.name ] } else Nothing
itemInlineUnique _ = Nothing

itemTableUnique :: TableItem -> Maybe { columns :: Array String }
itemTableUnique (TIUnique cs) = Just { columns: cs }
itemTableUnique _ = Nothing

dropTable :: Parser String Migration
dropTable = do
  keyword "drop"
  keyword "table"
  _ <- optionMaybe (keywords [ "if", "exists" ])
  name <- qualifiedName
  pure (DropTable name)

alterTable :: Parser String Migration
alterTable = do
  keyword "alter"
  keyword "table"
  name <- qualifiedName
  try (alterAdd name) <|> alterDrop name

alterAdd :: String -> Parser String Migration
alterAdd tName = do
  keyword "add"
  try (addColumn tName)
    <|> try (addForeignKey tName)
    <|> addUnnamedColumn tName
  where
  addColumn t = do
    keyword "column"
    _ <- optionMaybe (keywords [ "if", "not", "exists" ])
    cdef <- columnDef
    case cdef of
      TIColumn r -> pure (AddColumn t r.col)
      _ -> fail "ADD COLUMN: expected column definition"

  addForeignKey t = do
    _ <- optionMaybe (try (keyword "constraint" *> void identifier))
    keywords [ "foreign", "key" ]
    cols <- parens columnList
    keyword "references"
    refTable <- qualifiedName
    refColumns <- parens columnList
    actions <- referencesTail
    pure (AddForeignKey t
      { columns: cols
      , refTable
      , refColumns
      , onDelete: actions.onDelete
      , onUpdate: actions.onUpdate
      })

  addUnnamedColumn t = do
    cdef <- columnDef
    case cdef of
      TIColumn r -> pure (AddColumn t r.col)
      _ -> fail "ADD: expected column or FOREIGN KEY"

alterDrop :: String -> Parser String Migration
alterDrop tName = do
  keyword "drop"
  try (dropColumn tName)
    <|> dropForeignKey tName
  where
  dropColumn t = do
    keyword "column"
    _ <- optionMaybe (keywords [ "if", "exists" ])
    n <- identifier
    pure (DropColumn t n)

  dropForeignKey t = do
    keywords [ "foreign", "key" ]
    cols <- parens columnList
    pure (DropForeignKey t cols)
