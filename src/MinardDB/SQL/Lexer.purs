-- | Shared SQL lexer primitives.
-- |
-- | Both the DDL parser (`MinardDB.Migration.SQL`, Phase 3d) and the
-- | query parser (`MinardDB.Query.SQL`, Phase 4) tokenise SQL the same
-- | way: skip whitespace and `--` / `/* */` comments, read
-- | case-insensitive keywords, quoted-or-bare identifiers, punctuation,
-- | and integer literals. This module holds that token layer so the two
-- | grammars share one notion of "what a token is".
-- |
-- | The reserved-word set differs between the two grammars (DDL reserves
-- | CREATE/TABLE/…, queries reserve SELECT/FROM/…), so `identifierWith`
-- | takes the reserved predicate as an argument rather than baking one
-- | in. Quoted identifiers always bypass the reserved check, so a table
-- | literally named `"order"` still parses.
module MinardDB.SQL.Lexer
  ( isWsChar
  , isAsciiLetter
  , isAsciiDigit
  , isWordChar
  , skipFiller
  , lexeme
  , rawWord
  , keyword
  , keywords
  , identifierWith
  , symbol
  , integer
  , parens
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Char (toCharCode)
import Data.Foldable (traverse_)
import Data.Int as Int
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray)
import Data.String.Common (toLower)
import Parsing (Parser, fail)
import Parsing.Combinators (between, manyTill, try)
import Parsing.String (anyChar, char, eof, satisfy, string)

------------------------------------------------------------------------
-- Character classes
------------------------------------------------------------------------

isWsChar :: Char -> Boolean
isWsChar c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

isAsciiLetter :: Char -> Boolean
isAsciiLetter c =
  let n = toCharCode c
  in (n >= 65 && n <= 90) || (n >= 97 && n <= 122)

isAsciiDigit :: Char -> Boolean
isAsciiDigit c =
  let n = toCharCode c
  in n >= 48 && n <= 57

isWordChar :: Char -> Boolean
isWordChar c = isAsciiLetter c || isAsciiDigit c || c == '_'

------------------------------------------------------------------------
-- Whitespace + comments
------------------------------------------------------------------------

-- | Consume whitespace and `--` / `/* */` comments until a real token.
skipFiller :: Parser String Unit
skipFiller = go
  where
  go = do
    progress <-
          (satisfy isWsChar *> pure true)
      <|> try (lineCmt *> pure true)
      <|> try (blockCmt *> pure true)
      <|> pure false
    if progress then go else pure unit

  lineCmt = do
    _ <- string "--"
    _ <- manyTill anyChar (void (char '\n') <|> eof)
    pure unit

  blockCmt = do
    _ <- string "/*"
    _ <- manyTill anyChar (string "*/")
    pure unit

-- | Token wrapper: parse `p`, then eat trailing filler.
lexeme :: forall a. Parser String a -> Parser String a
lexeme p = p <* skipFiller

------------------------------------------------------------------------
-- Token primitives
------------------------------------------------------------------------

-- | A bare word starting with letter or `_`, continuing with word chars.
-- | Does NOT consume trailing whitespace.
rawWord :: Parser String String
rawWord = do
  c <- satisfy (\ch -> isAsciiLetter ch || ch == '_')
  cs <- Array.many (try (satisfy isWordChar))
  pure (fromCharArray (Array.cons c cs))

-- | Case-insensitive keyword. Matches a full word, fails (without
-- | committing) if the word doesn't equal `kw`.
keyword :: String -> Parser String Unit
keyword kw = lexeme $ try do
  w <- rawWord
  if toLower w == toLower kw then pure unit
  else fail ("expected `" <> kw <> "`")

-- | A multi-word keyword phrase: e.g. `keywords ["foreign", "key"]`.
-- | All-or-nothing: if any word fails, position is reset.
keywords :: Array String -> Parser String Unit
keywords ks = try (traverse_ keyword ks)

-- | An identifier: either a double-quoted name (any contents) or an
-- | unquoted word that is not reserved (per the supplied predicate).
identifierWith :: (String -> Boolean) -> Parser String String
identifierWith isReserved = lexeme (quoted <|> bare)
  where
  quoted = do
    _ <- char '"'
    cs <- Array.many (satisfy (_ /= '"'))
    _ <- char '"'
    pure (fromCharArray cs)
  bare = try do
    w <- rawWord
    if isReserved (toLower w)
      then fail ("unexpected keyword `" <> w <> "` where identifier expected")
      else pure w

-- | A literal punctuation token, with trailing whitespace eaten.
symbol :: String -> Parser String Unit
symbol s = lexeme (void (string s))

-- | A positive integer literal.
integer :: Parser String Int
integer = lexeme do
  ds <- Array.some (try (satisfy isAsciiDigit))
  case Int.fromString (fromCharArray ds) of
    Just n -> pure n
    Nothing -> fail "integer literal too large"

-- | `(p)`.
parens :: forall a. Parser String a -> Parser String a
parens = between (symbol "(") (symbol ")")
