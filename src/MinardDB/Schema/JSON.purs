module MinardDB.Schema.JSON where

import Prelude

import Data.Argonaut.Core (Json, toArray, toBoolean, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import MinardDB.Schema (Column, FDSource(..), FKAction(..), ForeignKey, FunctionalDependency, PGType(..), Schema, Table, UniqueConstraint)

-- | Parse a Schema-shaped JSON blob (produced by tools/introspect-duckdb.py)
-- | into a Schema value (declared FKs only).
parseSchema :: String -> Either String Schema
parseSchema text = parseSchemaFull text <#> _.declared

-- | An inferred FK with its source table preserved (the FK type itself
-- | doesn't track which table it lives on, since within a Table we
-- | already know).
type InferredFK =
  { sourceTable :: String
  , columns :: Array String
  , refTable :: String
  , refColumns :: Array String
  }

-- | Result of parsing a schema JSON file.
type ParsedSchema =
  { declared :: Schema
  , withInferred :: Schema  -- declared FKs + inferred FKs, merged for proof
  , inferredFKs :: Array InferredFK
  }

parseSchemaFull :: String -> Either String ParsedSchema
parseSchemaFull text = do
  json <- jsonParser text
  obj <- json # toObject # note "schema root is not an object"
  name <- objString obj "name"
  tablesJ <- objArray obj "tables"
  declaredTables <- traverse (parseTable false) tablesJ
  mergedTables <- traverse (parseTable true) tablesJ
  inferredFKs <- Array.concat <$> traverse extractInferredFKs tablesJ
  pure
    { declared: { name, tables: declaredTables }
    , withInferred: { name, tables: mergedTables }
    , inferredFKs
    }

-- | Pull the inferredForeignKeys array off a raw table JSON value,
-- | tagging each entry with its source table name.
extractInferredFKs :: Json -> Either String (Array InferredFK)
extractInferredFKs j = do
  obj <- j # toObject # note "table not an object"
  srcTable <- objString obj "name"
  case Object.lookup "inferredForeignKeys" obj of
    Nothing -> Right []
    Just ij -> case toArray ij of
      Nothing -> Right []
      Just arr -> traverse (parseInferred srcTable) arr

parseInferred :: String -> Json -> Either String InferredFK
parseInferred srcTable j = do
  obj <- j # toObject # note "inferredFK not an object"
  columns <- objStringArray obj "columns"
  refTable <- objString obj "refTable"
  refColumns <- objStringArray obj "refColumns"
  pure { sourceTable: srcTable, columns, refTable, refColumns }

-- | If `includeInferred` is true, merge `inferredForeignKeys` into
-- | `foreignKeys`. Otherwise only declared FKs are used.
parseTable :: Boolean -> Json -> Either String Table
parseTable includeInferred j = do
  obj <- j # toObject # note "table is not an object"
  name <- objString obj "name"
  schemaName <- objString obj "schemaName"
  columnsJ <- objArray obj "columns"
  columns <- traverse parseColumn columnsJ
  primaryKey <- objStringArray obj "primaryKey"
  fksJ <- objArray obj "foreignKeys"
  declaredFKs <- traverse parseFK fksJ
  inferredFKs <- case Object.lookup "inferredForeignKeys" obj of
    Nothing -> Right []
    Just ij -> case toArray ij of
      Just arr -> traverse parseFK arr
      Nothing -> Right []
  uqsJ <- objArray obj "uniqueConstraints"
  uniqueConstraints <- traverse parseUnique uqsJ
  functionalDependencies <- case Object.lookup "functionalDependencies" obj of
    Nothing -> Right []
    Just fdj -> case toArray fdj of
      Just arr -> traverse parseFD arr
      Nothing -> Right []
  let foreignKeys = if includeInferred
        then declaredFKs <> inferredFKs
        else declaredFKs
  pure { name, schemaName, columns, primaryKey, foreignKeys, uniqueConstraints, functionalDependencies }

parseColumn :: Json -> Either String Column
parseColumn j = do
  obj <- j # toObject # note "column is not an object"
  name <- objString obj "name"
  dt <- objString obj "dataType"
  dataType <- parsePGType dt
  nullable <- objBool obj "nullable"
  let defaultExpr = case Object.lookup "defaultExpr" obj of
        Just dj -> toString dj
        Nothing -> Nothing
  pure { name, dataType, nullable, defaultExpr }

parsePGType :: String -> Either String PGType
parsePGType = case _ of
  "PGInt" -> Right PGInt
  "PGBigInt" -> Right PGBigInt
  "PGText" -> Right PGText
  "PGBoolean" -> Right PGBoolean
  "PGTimestamp" -> Right PGTimestamp
  "PGDate" -> Right PGDate
  "PGUUID" -> Right PGUUID
  "PGJsonb" -> Right PGJsonb
  other -> Left ("unknown PGType: " <> other)

parseFK :: Json -> Either String ForeignKey
parseFK j = do
  obj <- j # toObject # note "foreignKey is not an object"
  columns <- objStringArray obj "columns"
  refTable <- objString obj "refTable"
  refColumns <- objStringArray obj "refColumns"
  onDelete <- objString obj "onDelete" >>= parseFKAction
  onUpdate <- objString obj "onUpdate" >>= parseFKAction
  pure { columns, refTable, refColumns, onDelete, onUpdate }

parseFKAction :: String -> Either String FKAction
parseFKAction = case _ of
  "Cascade" -> Right Cascade
  "SetNull" -> Right SetNull
  "Restrict" -> Right Restrict
  "NoAction" -> Right NoAction
  other -> Left ("unknown FKAction: " <> other)

parseUnique :: Json -> Either String UniqueConstraint
parseUnique j = do
  obj <- j # toObject # note "uniqueConstraint is not an object"
  columns <- objStringArray obj "columns"
  pure { columns }

parseFD :: Json -> Either String FunctionalDependency
parseFD j = do
  obj <- j # toObject # note "functionalDependency is not an object"
  determinant <- objStringArray obj "determinant"
  dependent <- objStringArray obj "dependent"
  -- For now we only accept Declared FDs from the introspector / fixtures.
  -- Inferred FDs (from data sampling) arrive in a later phase.
  source <- case Object.lookup "source" obj of
    Just sj -> case toString sj of
      Just "Declared" -> Right Declared
      Just other -> Left ("unsupported FD source: " <> other)
      Nothing -> Left "`source` is not a string"
    Nothing -> Right Declared  -- default
  pure { determinant, dependent, source }

-- Helpers -------------------------------------------------------------

objString :: Object Json -> String -> Either String String
objString obj key =
  Object.lookup key obj
    # note ("missing key: " <> key)
    >>= (\j -> j # toString # note ("`" <> key <> "` is not a string"))

objBool :: Object Json -> String -> Either String Boolean
objBool obj key =
  Object.lookup key obj
    # note ("missing key: " <> key)
    >>= (\j -> j # toBoolean # note ("`" <> key <> "` is not a boolean"))

objArray :: Object Json -> String -> Either String (Array Json)
objArray obj key =
  Object.lookup key obj
    # note ("missing key: " <> key)
    >>= (\j -> j # toArray # note ("`" <> key <> "` is not an array"))

objStringArray :: Object Json -> String -> Either String (Array String)
objStringArray obj key = do
  arr <- objArray obj key
  traverse (\j -> j # toString # note ("element of `" <> key <> "` is not a string")) arr

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg
