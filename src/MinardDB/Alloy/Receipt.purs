module MinardDB.Alloy.Receipt where

import Prelude

import Data.Argonaut.Core (Json, toArray, toObject, toString)
import Data.Argonaut.Parser (jsonParser)
import Data.Array as Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Data.Tuple (Tuple, snd)
import Foreign.Object (Object)
import Foreign.Object as Object

-- | Parsed result of a single Alloy command (check or run).
type CommandResult =
  { name :: String
  , kind :: CommandKind
  , source :: String     -- e.g. "check NoFKCycle for 5"
  , verdict :: Verdict
  }

data CommandKind = Check | Run

derive instance eqCommandKind :: Eq CommandKind
instance showCommandKind :: Show CommandKind where
  show Check = "check"
  show Run = "run"

-- | Verdict for a command.
-- |
-- | For a `check`:
-- |   NoCounterexample (UNSAT) = property holds within scope (the good case)
-- |   Counterexample   (SAT)   = Alloy found a violating instance (BROKEN)
-- |
-- | For a `run`:
-- |   NoCounterexample (UNSAT) = no satisfying instance exists
-- |   Counterexample   (SAT)   = found an instance (the good case)
data Verdict = NoCounterexample | Counterexample

derive instance eqVerdict :: Eq Verdict
instance showVerdict :: Show Verdict where
  show NoCounterexample = "UNSAT"
  show Counterexample = "SAT"

-- | Parse receipt.json into a list of command results.
parseReceipt :: String -> Either String (Array CommandResult)
parseReceipt jsonText = do
  json <- jsonParser jsonText
  rootObj <- json # toObject # note "receipt root is not an object"
  commandsJson <- Object.lookup "commands" rootObj # note "missing `commands`"
  commandsObj <- commandsJson # toObject # note "`commands` is not an object"
  let entries = Object.toUnfoldable commandsObj :: Array (Tuple String Json)
  traverse (parseCommand <<< snd) entries

parseCommand :: Json -> Either String CommandResult
parseCommand cmdJson = do
  obj <- cmdJson # toObject # note "command entry is not an object"
  name <- objString obj "name"
  kind <- objString obj "type" >>= parseKind
  source <- objString obj "source"
  verdict <- determineVerdict obj
  pure { name, kind, source, verdict }

parseKind :: String -> Either String CommandKind
parseKind = case _ of
  "check" -> Right Check
  "run" -> Right Run
  other -> Left ("unknown command type: " <> other)

-- | SAT iff solution[0].instances[] is non-empty.
determineVerdict :: Object Json -> Either String Verdict
determineVerdict obj =
  case Object.lookup "solution" obj of
    Nothing -> Right NoCounterexample
    Just sJson -> case toArray sJson of
      Nothing -> Right NoCounterexample
      Just solArr -> case Array.head solArr of
        Nothing -> Right NoCounterexample
        Just first -> do
          firstObj <- first # toObject # note "solution entry not an object"
          case Object.lookup "instances" firstObj of
            Nothing -> Right NoCounterexample
            Just iJson -> case toArray iJson of
              Just iArr | Array.length iArr > 0 -> Right Counterexample
              _ -> Right NoCounterexample

objString :: Object Json -> String -> Either String String
objString obj key =
  Object.lookup key obj
    # note ("missing key: " <> key)
    >>= (\j -> j # toString # note ("`" <> key <> "` is not a string"))

note :: forall a. String -> Maybe a -> Either String a
note msg = case _ of
  Just x -> Right x
  Nothing -> Left msg
