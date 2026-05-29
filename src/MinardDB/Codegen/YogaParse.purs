-- | Reverse bridge: parse rowtype-yoga type-level `Table` declarations
-- | back into a notothenia `Schema` (the other half of extension-ladder
-- | rung 1; see `docs/SYNTHESIS.md`).
-- |
-- | Pairs with `MinardDB.Codegen.YogaTable` (the forward direction).
-- | Together they make the Schema AST the pivot between the value-world
-- | (notothenia/Alloy) and the type-world (yoga `Table`/`Q`). Parsing
-- | hand-written `Table` declarations back into a `Schema` and diffing
-- | against the introspected/parsed catalog (`MinardDB.Schema.Diff`)
-- | gives **drift detection**: the typed bindings disagreeing with the
-- | real database is a real bug.
-- |
-- | Scope: parses the `type Foo = Table "foo" ( … )` form this project's
-- | generator emits — the wrapper vocabulary PrimaryKey / AutoIncrement /
-- | Unique / Nullable / Default / DefaultExpr / ForeignKey…References,
-- | over the base types `pgToPs` produces. It scans a whole module,
-- | ignoring module/import lines and anything that isn't a `Table`
-- | declaration. Reuses the shared SQL token layer (`MinardDB.SQL.Lexer`)
-- | for whitespace/`--`-comments/words/parens.
module MinardDB.Codegen.YogaParse
  ( parseYogaTables
  , parseYogaSchema
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import MinardDB.SQL.Lexer (lexeme, parens, rawWord, skipFiller, symbol)
import MinardDB.Schema (Column, FKAction(..), ForeignKey, PGType(..), Schema, Table, UniqueConstraint)
import Parsing (Parser, fail, parseErrorMessage, runParser)
import Parsing.Combinators (optionMaybe, sepBy, try)
import Parsing.String (anyChar, char, eof, satisfy)

parseYogaSchema :: String -> String -> Either String Schema
parseYogaSchema name src = parseYogaTables src <#> \tables -> { name, tables }

parseYogaTables :: String -> Either String (Array Table)
parseYogaTables src = case runParser src program of
  Left e -> Left (parseErrorMessage e)
  Right ts -> Right ts
  where
  -- Scan the module: collect every `Table` declaration, skip the rest
  -- (module header, imports, comments) one token at a time.
  program = do
    skipFiller
    ts <- gather []
    pure ts

  gather acc = (eof *> pure acc) <|> do
    m <- optionMaybe (try tableDecl)
    case m of
      Just t -> gather (Array.snoc acc t)
      Nothing -> skipOne *> gather acc

  skipOne = (void stringLit <|> void rawWord <|> void anyChar) *> skipFiller

------------------------------------------------------------------------
-- Tokens
------------------------------------------------------------------------

ident :: Parser String String
ident = lexeme rawWord

-- | Exact (case-sensitive) keyword — PureScript is case-sensitive, so we
-- | don't use the SQL lexer's case-insensitive `keyword`.
kw :: String -> Parser String Unit
kw s = lexeme $ try do
  w <- rawWord
  if w == s then pure unit else fail ("expected `" <> s <> "`")

stringLit :: Parser String String
stringLit = lexeme do
  _ <- char '"'
  cs <- Array.many (satisfy (_ /= '"'))
  _ <- char '"'
  pure (fromCharArray cs)

------------------------------------------------------------------------
-- Declaration
------------------------------------------------------------------------

tableDecl :: Parser String Table
tableDecl = do
  kw "type"
  _ <- ident          -- the alias name, e.g. ProjectsTable (discarded)
  symbol "="
  kw "Table"
  sqlName <- stringLit
  fields <- parens (sepBy field (symbol ","))
  pure (assemble sqlName (Array.fromFoldable fields))

type Field = { name :: String, attrs :: Attrs }

field :: Parser String Field
field = do
  name <- ident
  symbol "::"
  attrs <- typeExpr
  pure { name, attrs }

------------------------------------------------------------------------
-- Column attributes accumulated from the wrapper chain
------------------------------------------------------------------------

type Attrs =
  { pgType :: PGType
  , pk :: Boolean
  , autoInc :: Boolean
  , unique :: Boolean
  , nullable :: Boolean
  , default :: Maybe { expr :: Boolean, text :: String }
  , fk :: Maybe { refTable :: String, refCol :: String }
  }

emptyAttrs :: PGType -> Attrs
emptyAttrs pg =
  { pgType: pg, pk: false, autoInc: false, unique: false
  , nullable: false, default: Nothing, fk: Nothing }

-- | A type expression: either a base type (terminal) or a wrapper
-- | applied to an operand. The generator parenthesises multi-token
-- | operands and leaves single-token (base) operands bare.
typeExpr :: Parser String Attrs
typeExpr = do
  h <- ident
  case baseType h of
    Just pg -> pure (emptyAttrs pg)
    Nothing -> do
      apply <- wrapperArgs h
      operand <- parens typeExpr <|> baseOperand
      pure (apply operand)

baseOperand :: Parser String Attrs
baseOperand = do
  h <- ident
  case baseType h of
    Just pg -> pure (emptyAttrs pg)
    Nothing -> fail ("expected a base type, got `" <> h <> "`")

-- | Given a wrapper head, parse any args it carries and return a
-- | function that stamps its flag onto the operand's attrs.
wrapperArgs :: String -> Parser String (Attrs -> Attrs)
wrapperArgs = case _ of
  "PrimaryKey" -> pure (_ { pk = true })
  "AutoIncrement" -> pure (_ { autoInc = true })
  "Unique" -> pure (_ { unique = true })
  "Nullable" -> pure (_ { nullable = true })
  "Default" -> do
    s <- stringLit
    pure (_ { default = Just { expr: false, text: s } })
  "DefaultExpr" -> do
    s <- stringLit
    pure (_ { default = Just { expr: true, text: s } })
  "ForeignKey" -> do
    refTable <- stringLit
    kw "References"
    refCol <- stringLit
    pure (_ { fk = Just { refTable, refCol } })
  other -> fail ("unknown column wrapper `" <> other <> "`")

baseType :: String -> Maybe PGType
baseType = case _ of
  "Int" -> Just PGInt
  "String" -> Just PGText
  "Boolean" -> Just PGBoolean
  "DateTime" -> Just PGTimestamp
  "PGDate" -> Just PGDate
  "PGUUID" -> Just PGUUID
  "Jsonb" -> Just PGJsonb
  "Number" -> Just PGDecimal
  "Foreign" -> Just PGBlob
  _ -> Nothing

------------------------------------------------------------------------
-- Assemble a notothenia Table
------------------------------------------------------------------------

assemble :: String -> Array Field -> Table
assemble name fields =
  { name
  , schemaName: "main"
  , columns: map toColumn fields
  , primaryKey: Array.mapMaybe (\f -> if f.attrs.pk then Just f.name else Nothing) fields
  , foreignKeys: Array.mapMaybe toFK fields
  , uniqueConstraints: Array.mapMaybe toUnique fields
  , functionalDependencies: []
  }
  where
  toColumn :: Field -> Column
  toColumn f =
    { name: f.name
    , dataType: f.attrs.pgType
    , nullable: f.attrs.nullable
    , defaultExpr: defaultText f.attrs
    }

  defaultText a =
    if a.autoInc then Just "nextval(...)"
    else case a.default of
      Just d -> Just d.text
      Nothing -> Nothing

  toUnique :: Field -> Maybe UniqueConstraint
  toUnique f = if f.attrs.unique then Just { columns: [ f.name ] } else Nothing

  toFK :: Field -> Maybe ForeignKey
  toFK f = f.attrs.fk <#> \r ->
    { columns: [ f.name ]
    , refTable: r.refTable
    , refColumns: [ r.refCol ]
    , onDelete: NoAction
    , onUpdate: NoAction
    }
