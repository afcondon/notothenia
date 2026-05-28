module MinardDB.Schema where

import Prelude

import Data.Maybe (Maybe)

type Schema =
  { name :: String
  , tables :: Array Table
  }

type Table =
  { name :: String
  , schemaName :: String
  , columns :: Array Column
  , primaryKey :: Array String
  , foreignKeys :: Array ForeignKey
  , uniqueConstraints :: Array UniqueConstraint
  , functionalDependencies :: Array FunctionalDependency
  }

type Column =
  { name :: String
  , dataType :: PGType
  , nullable :: Boolean
  , defaultExpr :: Maybe String
  }

data PGType
  = PGInt
  | PGBigInt
  | PGText
  | PGVarchar Int
  | PGBoolean
  | PGTimestamp
  | PGDate
  | PGUUID
  | PGJsonb

derive instance eqPGType :: Eq PGType
instance showPGType :: Show PGType where
  show = case _ of
    PGInt -> "PGInt"
    PGBigInt -> "PGBigInt"
    PGText -> "PGText"
    PGVarchar n -> "PGVarchar " <> show n
    PGBoolean -> "PGBoolean"
    PGTimestamp -> "PGTimestamp"
    PGDate -> "PGDate"
    PGUUID -> "PGUUID"
    PGJsonb -> "PGJsonb"

type ForeignKey =
  { columns :: Array String
  , refTable :: String
  , refColumns :: Array String
  , onDelete :: FKAction
  , onUpdate :: FKAction
  }

data FKAction
  = Cascade
  | SetNull
  | Restrict
  | NoAction

derive instance eqFKAction :: Eq FKAction
instance showFKAction :: Show FKAction where
  show = case _ of
    Cascade -> "Cascade"
    SetNull -> "SetNull"
    Restrict -> "Restrict"
    NoAction -> "NoAction"

type UniqueConstraint =
  { columns :: Array String }

-- | A functional dependency: determinant → dependent.
-- | Source distinguishes proof material (Declared) from candidates (Inferred).
type FunctionalDependency =
  { determinant :: Array String
  , dependent :: Array String
  , source :: FDSource
  }

data FDSource
  = Declared
  | Inferred { sampleSize :: Int, holdsRatio :: Number }

derive instance eqFDSource :: Eq FDSource
