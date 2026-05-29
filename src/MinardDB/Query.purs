-- | Parse a SQL query into the table/column references it touches
-- | (Phase 4 — query reach analysis).
-- |
-- | Reach analysis doesn't need a full SQL query semantics — it needs to
-- | know *which tables and columns a query references*. So rather than
-- | build a complete expression grammar (CASE, arithmetic, window
-- | functions, scalar subqueries — a genuine rabbit hole), we tokenise
-- | the query and harvest references heuristically:
-- |
-- |   * FROM / JOIN clauses give the table references (with aliases).
-- |   * Qualified names `t.c` and `t.*` are column references whose
-- |     qualifier is a table or alias.
-- |   * Bare names that aren't tables, aliases, keywords, function
-- |     names, or output aliases are column references with an unknown
-- |     qualifier — `MinardDB.Query.Reach` resolves them against the
-- |     schema by the SQL unambiguity rule.
-- |   * `*` is a wildcard over the FROM tables.
-- |
-- | This is deliberately approximate and errs toward *over*-collecting
-- | column references — which is the safe direction for dead-column
-- | detection (a column we wrongly think is referenced is merely "not
-- | flagged dead", never "wrongly flagged dead").
-- |
-- | What we do NOT handle: subqueries in FROM (a `(SELECT …) alias` is
-- | skipped, so its inner tables are missed), correlated subqueries,
-- | CTEs (the WITH body is harvested but the CTE name leaks in as a
-- | pseudo-table), and set operations beyond token harvesting. These are
-- | honest gaps; the parser never fails on them, it just under-reports.
module MinardDB.Query
  ( QueryKind(..)
  , TableRef
  , ColumnRef
  , QueryRefs
  , Token(..)
  , parseQuery
  , tokenize
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.Common (toLower)
import MinardDB.SQL.Lexer (rawWord, skipFiller)
import Parsing (Parser, parseErrorMessage, runParser)
import Parsing.Combinators (choice, lookAhead, manyTill, optionMaybe)
import Parsing.String (anyChar, char, eof, satisfy)

------------------------------------------------------------------------
-- Reference model
------------------------------------------------------------------------

data QueryKind = Select | Insert | Update | Delete | Other

derive instance eqQueryKind :: Eq QueryKind
instance showQueryKind :: Show QueryKind where
  show = case _ of
    Select -> "SELECT"
    Insert -> "INSERT"
    Update -> "UPDATE"
    Delete -> "DELETE"
    Other -> "OTHER"

-- | A table reference from a FROM/JOIN clause. `alias` is the
-- | correlation name if one was given (`users u` or `users AS u`).
type TableRef = { table :: String, alias :: Maybe String }

-- | A column reference. `qualifier` is the table-or-alias prefix when
-- | the reference was written `q.column`; Nothing for a bare column.
-- | `column` is `"*"` for a wildcard.
type ColumnRef = { qualifier :: Maybe String, column :: String }

type QueryRefs =
  { kind :: QueryKind
  , tables :: Array TableRef
  , columns :: Array ColumnRef
  }

------------------------------------------------------------------------
-- Tokens
------------------------------------------------------------------------

data Token
  = TKw String           -- a reserved word, lower-cased
  | TName String         -- a bare identifier (original case)
  | TQual String String  -- qualified `q.n`; n may be "*"
  | TStar                -- a bare `*`
  | TFunc String         -- identifier immediately followed by `(`
  | TComma
  | TLParen
  | TRParen
  | TOther               -- any other single character (operators, numbers, …)

derive instance eqToken :: Eq Token
instance showToken :: Show Token where
  show = case _ of
    TKw s -> "kw:" <> s
    TName s -> "name:" <> s
    TQual q n -> "qual:" <> q <> "." <> n
    TStar -> "*"
    TFunc s -> "func:" <> s
    TComma -> ","
    TLParen -> "("
    TRParen -> ")"
    TOther -> "·"

-- | Reserved words the query grammar recognises syntactically. A word
-- | not in this set (and not immediately before `(`) is treated as an
-- | identifier — a table, alias, or column.
isReserved :: String -> Boolean
isReserved w = Array.elem w
  [ "select", "from", "where", "join", "inner", "left", "right", "full"
  , "outer", "cross", "natural", "on", "using", "group", "by", "order"
  , "having", "limit", "offset", "as", "and", "or", "not", "in", "is"
  , "null", "like", "ilike", "between", "distinct", "all", "union"
  , "intersect", "except", "asc", "desc", "case", "when", "then", "else"
  , "end", "exists", "with", "insert", "into", "values", "update", "set"
  , "delete", "returning", "true", "false"
  ]

------------------------------------------------------------------------
-- Tokenizer
------------------------------------------------------------------------

tokenize :: String -> Either String (Array Token)
tokenize src = case runParser src program of
  Left e -> Left (parseErrorMessage e)
  Right toks -> Right toks
  where
  program = skipFiller *> Array.many token <* eof

token :: Parser String Token
token = choice
  [ stringLit
  , wordToken
  , punct '*' TStar
  , punct ',' TComma
  , punct '(' TLParen
  , punct ')' TRParen
  , otherChar
  ]

-- | A word: keyword, function name, qualified name, or plain identifier.
-- | We must inspect what immediately follows the word (a `.` or `(`)
-- | *before* eating trailing whitespace, so qualified names and function
-- | calls are recognised.
wordToken :: Parser String Token
wordToken = do
  w <- rawWord
  mDot <- optionMaybe (char '.')
  case mDot of
    Just _ -> do
      sub <- (char '*' *> pure "*") <|> rawWord
      skipFiller
      pure (TQual w sub)
    Nothing -> do
      isFunc <- (lookAhead (char '(') *> pure true) <|> pure false
      skipFiller
      let lw = toLower w
      pure
        if isFunc then TFunc lw
        else if isReserved lw then TKw lw
        else TName w

stringLit :: Parser String Token
stringLit = do
  _ <- char '\''
  _ <- manyTill anyChar (char '\'')
  skipFiller
  pure TOther

punct :: Char -> Token -> Parser String Token
punct c t = char c *> skipFiller *> pure t

-- | Catch-all: consume one character (operator, digit, etc.) as TOther.
otherChar :: Parser String Token
otherChar = do
  _ <- satisfy (const true)
  skipFiller
  pure TOther

------------------------------------------------------------------------
-- Extraction
------------------------------------------------------------------------

parseQuery :: String -> Either String QueryRefs
parseQuery src = do
  toks <- tokenize src
  let tables = extractTables toks
  pure
    { kind: kindOf toks
    , tables
    , columns: extractColumns toks tables
    }

kindOf :: Array Token -> QueryKind
kindOf toks = case Array.find isKw toks of
  Just (TKw "select") -> Select
  Just (TKw "insert") -> Insert
  Just (TKw "update") -> Update
  Just (TKw "delete") -> Delete
  _ -> Other
  where
  isKw = case _ of
    TKw _ -> true
    _ -> false

------------------------------------------------------------------------
-- Table extraction: a small state machine over the token stream.
------------------------------------------------------------------------

data TblState
  = Scan                -- not in a table position
  | ExpectTable         -- just saw FROM / JOIN / INTO / UPDATE
  | ExpectAlias         -- just read a table; an alias / comma / join may follow

-- | Walk the token stream collecting FROM/JOIN/INTO/UPDATE targets.
-- | A `JOIN` keyword opens a single table position; `FROM`/`INTO`/
-- | `UPDATE` open a comma-separated list. A `(` in table position is a
-- | subquery we don't descend into — we drop back to Scan and miss its
-- | inner tables (a documented gap).
extractTables :: Array Token -> Array TableRef
extractTables = walk Scan []
  where
  walk :: TblState -> Array TableRef -> Array Token -> Array TableRef
  walk state acc toks = case Array.uncons toks of
    Nothing -> Array.reverse acc
    Just { head: tok, tail } -> case state, tok of
      -- Keywords that open a table position (from any state).
      _, TKw "from" -> walk ExpectTable acc tail
      _, TKw "into" -> walk ExpectTable acc tail
      _, TKw "update" -> walk ExpectTable acc tail
      _, TKw kw | isJoinWord kw -> walk ExpectTable acc tail

      -- In a table position: record the table.
      ExpectTable, TName t -> walk ExpectAlias (push t acc) tail
      ExpectTable, TQual _ t -> walk ExpectAlias (push t acc) tail
      ExpectTable, _ -> walk Scan acc tail  -- e.g. `(` subquery — skip

      -- After a table: an explicit/implicit alias, a comma (next table),
      -- or anything else ends the from-list.
      ExpectAlias, TKw "as" -> walk ExpectAlias acc tail  -- alias name comes next
      ExpectAlias, TComma -> walk ExpectTable acc tail
      ExpectAlias, TName a -> walk Scan (setAlias a acc) tail
      ExpectAlias, _ -> walk Scan acc tail

      Scan, _ -> walk Scan acc tail

  isJoinWord :: String -> Boolean
  isJoinWord = case _ of
    "join" -> true
    "inner" -> true
    "left" -> true
    "right" -> true
    "full" -> true
    "cross" -> true
    _ -> false

  push :: String -> Array TableRef -> Array TableRef
  push t acc = Array.cons { table: t, alias: Nothing } acc

  -- The most-recently pushed table is at the head of the reversed acc.
  setAlias :: String -> Array TableRef -> Array TableRef
  setAlias a acc = case Array.uncons acc of
    Just { head, tail } -> Array.cons (head { alias = Just a }) tail
    Nothing -> acc

------------------------------------------------------------------------
-- Column extraction
------------------------------------------------------------------------

-- | Harvest column references. Qualified names and stars are taken
-- | verbatim; bare names are kept only if they're not a table name, a
-- | table alias, or an output alias (the name right after `AS`). We
-- | track the previous token to spot the AS case.
extractColumns :: Array Token -> Array TableRef -> Array ColumnRef
extractColumns toks tables = dedup (go Nothing [] toks)
  where
  tableNames = map _.table tables
  aliases = Array.mapMaybe _.alias tables
  excluded = Array.nub (tableNames <> aliases)

  go :: Maybe Token -> Array ColumnRef -> Array Token -> Array ColumnRef
  go prev acc ts = case Array.uncons ts of
    Nothing -> Array.reverse acc
    Just { head: tok, tail } -> case tok of
      TQual q "*" -> go (Just tok) (Array.cons { qualifier: Just q, column: "*" } acc) tail
      TQual q c -> go (Just tok) (Array.cons { qualifier: Just q, column: c } acc) tail
      TStar -> go (Just tok) (Array.cons { qualifier: Nothing, column: "*" } acc) tail
      TName w ->
        let
          skip = isAfterAs prev || Array.elem w excluded
        in
          if skip then go (Just tok) acc tail
          else go (Just tok) (Array.cons { qualifier: Nothing, column: w } acc) tail
      _ -> go (Just tok) acc tail

  isAfterAs = case _ of
    Just (TKw "as") -> true
    _ -> false

  dedup :: Array ColumnRef -> Array ColumnRef
  dedup = Array.nubByEq \a b -> a.qualifier == b.qualifier && a.column == b.column
