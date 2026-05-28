module MinardDB.Alloy.Names
  ( sigName
  , fieldName
  , sanitize
  ) where

import Prelude

import Data.String.CodeUnits as CodeUnits

-- | Alloy sig names must be valid identifiers. We sanitize non-alphanumeric
-- | characters to underscores.
sigName :: String -> String
sigName = sanitize

fieldName :: String -> String
fieldName = sanitize

sanitize :: String -> String
sanitize s =
  CodeUnits.fromCharArray (map replace (CodeUnits.toCharArray s))
  where
    replace c =
      if isAlphaNumOrUnderscore c then c else '_'

isAlphaNumOrUnderscore :: Char -> Boolean
isAlphaNumOrUnderscore c =
  (c >= 'a' && c <= 'z')
    || (c >= 'A' && c <= 'Z')
    || (c >= '0' && c <= '9')
    || c == '_'
