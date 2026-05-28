module MinardDB.Schema.JSON where

import Prelude

import Data.Argonaut.Core (Json, toArray, toBoolean, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Foreign.Object (Object)
import Foreign.Object as Object
import MinardDB.Schema (Column, FKAction(..), ForeignKey, PGType(..), Schema, Table, UniqueConstraint)

-- | Parse a Schema-shaped JSON blob (produced by tools/introspect-duckdb.py)
-- | into a Schema value.
parseSchema :: String -> Either String Schema
parseSchema text = do
  json <- jsonParser text
  obj <- json # toObject # note "schema root is not an object"
  name <- objString obj "name"
  tablesJ <- objArray obj "tables"
  tables <- traverse parseTable tablesJ
  pure { name, tables }

parseTable :: Json -> Either String Table
parseTable j = do
  obj <- j # toObject # note "table is not an object"
  name <- objString obj "name"
  schemaName <- objString obj "schemaName"
  columnsJ <- objArray obj "columns"
  columns <- traverse parseColumn columnsJ
  primaryKey <- objStringArray obj "primaryKey"
  fksJ <- objArray obj "foreignKeys"
  foreignKeys <- traverse parseFK fksJ
  uqsJ <- objArray obj "uniqueConstraints"
  uniqueConstraints <- traverse parseUnique uqsJ
  pure { name, schemaName, columns, primaryKey, foreignKeys, uniqueConstraints }

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
