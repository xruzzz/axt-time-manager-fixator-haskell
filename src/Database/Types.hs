{-# LANGUAGE Arrows, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, UnicodeSyntax #-}
module Database.Types
    (
        AffairType,
        AffairTypeColumns,
        AffairTypePG,
        QueryAffairType
    ) where

-- import Database.PostgreSQL.Simple.Types ( PGArray)
import GHC.Generics
import Control.Arrow
import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Data.Profunctor.Product (p2, p3, p6)
import Data.Profunctor.Product.Default (Default(..))
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)

import Opaleye (Column, Nullable, PGArray, PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool, Query, QueryArr, QueryRunner, QueryRunnerColumnDefault(..), Table(Table), Unpackspec,
                            fieldQueryRunnerColumn, matchNullable, isNull,
                            required, queryTable,
                            restrict, (.==), (.<=), (.&&), (.<),
                            (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                            count, avg, sum, leftJoin, runQuery, showSqlForPostgres)

import Database.PostgreSQL.Simple.FromField (FromField(..))


type PlannerAffairType = (Integer,Integer,Integer,Maybe (PGArray Integer), String)

newtype BaseId = BaseId Int deriving (Show)

instance FromField BaseId where
  fromField field bs = BaseId <$> fromField field bs

type PgIC = Column PGInt4

instance QueryRunnerColumnDefault PGInt4 BaseId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn
type AffairType = AffairTypePoly BaseId Int Int [Int] String
type AffairTypeColumns = ( PgIC, PgIC, PgIC, Column (PGArray PGInt4), Column PGText)
type QueryAffairType = Query AffairTypePG

data AffairTypePoly idn cd prio asc lbl = AffairType {idn ∷ idn, code ∷ cd, priority ∷ prio, activity_specific_codes ∷ asc, label ∷ lbl}
    deriving Show
type AffairTypePG = AffairTypePoly PgIC PgIC PgIC (Column (PGArray PGInt4)) (Column PGText)

$(makeAdaptorAndInstance "pAffairType" ''AffairTypePoly)
