-- | Declared-intent layer — invariants the schema doesn't (or can't)
-- | encode, written down durably in an `.intent` file and merged into the
-- | `Schema` before checking.
-- |
-- | Motivation (the fog-of-war gap from `project_linting_engine_vision`):
-- | the schema and the compiler only see what the artifact encodes. Real
-- | intent that escaped the artifact — an FK disabled because DuckDB
-- | won't enforce it, a functional dependency no catalog can store, a
-- | "this self-reference is prevented by application logic" decision —
-- | dies at the next context seam. The `.intent` file is where that
-- | intent survives, in-repo and re-derivable, and the checker reasons
-- | about it.
-- |
-- | Two kinds of directive:
-- |
-- |   * **Declared structure** — `fk`, `fd`, `unique`. Merged into the
-- |     `Schema` so *every* tier benefits: an asserted `fk` reappears in
-- |     acyclicity/drift; an asserted `fd` is exactly what BCNF needs to
-- |     stop being vacuous (no catalog stores non-key FDs, so this is the
-- |     only way to make BCNF bite on a real schema).
-- |   * **Waivers** — `waive <check>: <reason>`. A finding the user has
-- |     acknowledged as acceptable (the `parent_id = id` self-parent that
-- |     app logic prevents). Downgrades a matching failure to a noted,
-- |     reason-carrying finding.
-- |
-- | The intent file can itself rot, so the checker also reports **stale
-- | assertions** (a directive naming a table/column that no longer
-- | exists) and **stale waivers** (matching no current failure). Fog of
-- | war, applied to the fog map.
-- |
-- | Format (one directive per line; `--` and `#` start comments;
-- | `--` may also trail a directive):
-- |
-- |   fk projects.parent_id -> projects.id
-- |   fd addresses: zip -> city, state
-- |   unique projects(slug)
-- |   waive NoFKCycle: parent_id self-reference prevented by app logic
module MinardDB.Intent
  ( Directive(..)
  , Waiver
  , IntentError
  , parseIntent
  , applyIntent
  , intentErrors
  , waivers
  , matchWaiver
  ) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Data.Array as Array
import Data.Bifunctor (lmap)
import Data.Either (Either)
import Data.Maybe (Maybe(..), isJust)
import Data.String (Pattern(..), stripPrefix)
import Data.String.CodeUnits (fromCharArray)
import Data.String.CodeUnits as SCU
import Data.String.Common (split, trim)
import Data.Traversable (traverse)
import MinardDB.SQL.Lexer (identifierWith, isWordChar, keyword, lexeme, parens, skipFiller, symbol)
import MinardDB.Schema (FDSource(..), FKAction(..), Schema)
import Parsing (Parser, parseErrorMessage, runParser)
import Parsing.Combinators (sepBy1)
import Parsing.String (anyChar, eof, satisfy)

------------------------------------------------------------------------
-- Model
------------------------------------------------------------------------

data Directive
  = AssertFK { table :: String, column :: String, refTable :: String, refColumn :: String }
  | AssertFD { table :: String, determinant :: Array String, dependent :: Array String }
  | AssertUnique { table :: String, columns :: Array String }
  | Waive { check :: String, reason :: String }

type Waiver = { check :: String, reason :: String }

type IntentError = { check :: String, detail :: String }

------------------------------------------------------------------------
-- Parsing (line-oriented)
------------------------------------------------------------------------

-- | Parse an intent file into directives. Blank lines and full-line
-- | `--`/`#` comments are dropped; every remaining line must parse.
parseIntent :: String -> Either String (Array Directive)
parseIntent src = traverse parseLine contentLines
  where
  contentLines =
    Array.filter (not <<< isBlankOrComment)
      (map trim (split (Pattern "\n") src))
  isBlankOrComment l =
    l == "" || startsWith "--" l || startsWith "#" l

parseLine :: String -> Either String Directive
parseLine line =
  lmap (\e -> "intent: " <> parseErrorMessage e <> " in line: " <> line)
    (runParser line (skipFiller *> directive <* eof))

directive :: Parser String Directive
directive = fkP <|> fdP <|> uniqueP <|> waiveP

fkP :: Parser String Directive
fkP = do
  keyword "fk"
  table <- ident
  symbol "."
  column <- ident
  symbol "->"
  refTable <- ident
  symbol "."
  refColumn <- ident
  pure (AssertFK { table, column, refTable, refColumn })

fdP :: Parser String Directive
fdP = do
  keyword "fd"
  table <- ident
  symbol ":"
  determinant <- identList
  symbol "->"
  dependent <- identList
  pure (AssertFD { table, determinant, dependent })

uniqueP :: Parser String Directive
uniqueP = do
  keyword "unique"
  table <- ident
  columns <- parens identList
  pure (AssertUnique { table, columns })

waiveP :: Parser String Directive
waiveP = do
  keyword "waive"
  check <- checkName
  symbol ":"
  reason <- restOfLine
  pure (Waive { check, reason })

-- | Table / column identifier (nothing is reserved in an intent file).
ident :: Parser String String
ident = identifierWith (const false)

identList :: Parser String (Array String)
identList = Array.fromFoldable <$> sepBy1 ident (symbol ",")

-- | A check/finding name: word chars plus the punctuation that appears in
-- | finding loci (`fk-acyclicity`, `BCNF_addresses_zip__city`,
-- | `fk-target/a.b_id`), so a waiver can name any of them.
checkName :: Parser String String
checkName = lexeme do
  cs <- Array.some (satisfy isCheckChar)
  pure (fromCharArray cs)
  where
  isCheckChar c = isWordChar c || c == '-' || c == '/' || c == '.'

restOfLine :: Parser String String
restOfLine = do
  cs <- Array.many anyChar
  pure (trim (fromCharArray cs))

------------------------------------------------------------------------
-- Apply structure to a schema
------------------------------------------------------------------------

-- | Merge the declared structure (FKs / FDs / uniques) into the schema,
-- | de-duplicating against what's already declared. Waivers are not
-- | structural and are ignored here.
applyIntent :: Array Directive -> Schema -> Schema
applyIntent dirs schema = schema { tables = map augment schema.tables }
  where
  augment t = t
    { foreignKeys = t.foreignKeys <> Array.filter (isNewFk t) (fksFor t.name)
    , functionalDependencies =
        t.functionalDependencies <> Array.filter (isNewFd t) (fdsFor t.name)
    , uniqueConstraints =
        t.uniqueConstraints <> Array.filter (isNewUq t) (uqsFor t.name)
    }

  fksFor name = Array.mapMaybe (toFk name) dirs
  fdsFor name = Array.mapMaybe (toFd name) dirs
  uqsFor name = Array.mapMaybe (toUq name) dirs

  toFk name = case _ of
    AssertFK r | r.table == name ->
      Just
        { columns: [ r.column ]
        , refTable: r.refTable
        , refColumns: [ r.refColumn ]
        , onDelete: NoAction
        , onUpdate: NoAction
        }
    _ -> Nothing

  toFd name = case _ of
    AssertFD r | r.table == name ->
      Just { determinant: r.determinant, dependent: r.dependent, source: Declared }
    _ -> Nothing

  toUq name = case _ of
    AssertUnique r | r.table == name -> Just { columns: r.columns }
    _ -> Nothing

  isNewFk t fk =
    not (Array.any (\e -> e.columns == fk.columns && e.refTable == fk.refTable) t.foreignKeys)
  isNewFd t fd =
    not (Array.any (\e -> e.determinant == fd.determinant && e.dependent == fd.dependent) t.functionalDependencies)
  isNewUq t uq =
    not (Array.any (\e -> e.columns == uq.columns) t.uniqueConstraints)

------------------------------------------------------------------------
-- Validate intent against the schema (catch stale assertions)
------------------------------------------------------------------------

-- | Structural directives that reference a table or column the schema
-- | doesn't have — the intent file has drifted from the schema.
intentErrors :: Array Directive -> Schema -> Array IntentError
intentErrors dirs schema = Array.mapMaybe check dirs
  where
  check = case _ of
    AssertFK r -> validate r.table [ r.column ] ("fk " <> r.table <> "." <> r.column)
    AssertFD r -> validate r.table (r.determinant <> r.dependent) ("fd on " <> r.table)
    AssertUnique r -> validate r.table r.columns ("unique on " <> r.table)
    Waive _ -> Nothing

  validate tname cols label = case findTable tname of
    Nothing ->
      Just { check: "intent-error", detail: label <> ": no such table `" <> tname <> "`" }
    Just t ->
      let missing = Array.filter (\c -> not (columnExists t c)) cols
      in
        if Array.null missing then Nothing
        else Just
          { check: "intent-error"
          , detail: label <> ": unknown column(s) " <> show missing <> " in `" <> tname <> "`"
          }

  findTable n = Array.find (\t -> t.name == n) schema.tables
  columnExists t c = Array.any (\col -> col.name == c) t.columns

------------------------------------------------------------------------
-- Waivers
------------------------------------------------------------------------

waivers :: Array Directive -> Array Waiver
waivers = Array.mapMaybe case _ of
  Waive w -> Just w
  _ -> Nothing

-- | Does a waiver's check name match a finding's check name? Exact, or a
-- | prefix terminated by a separator (`_`/`-`/`/`) — so `waive BCNF`
-- | covers every `BCNF_…` per-FD finding, while `waive fk-acyclicity`
-- | stays specific.
matchWaiver :: String -> String -> Boolean
matchWaiver waiverCheck findingCheck =
  waiverCheck == findingCheck ||
    case stripPrefix (Pattern waiverCheck) findingCheck of
      Just rest -> case SCU.charAt 0 rest of
        Just c -> c == '_' || c == '-' || c == '/'
        Nothing -> true
      Nothing -> false

------------------------------------------------------------------------
-- helpers
------------------------------------------------------------------------

startsWith :: String -> String -> Boolean
startsWith p s = isJust (stripPrefix (Pattern p) s)
