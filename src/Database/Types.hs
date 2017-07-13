{-# LANGUAGE Arrows, DeriveGeneric, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings, TemplateHaskell, UnicodeSyntax #-}
module Database.Types
    (
        BaseId(..),
        ListAffairType,
        ListAffairTypeColumns,
        ListAffairTypePGR,
        ListAffairTypePGW,
        ListAffairTypePoly(..),
        QueryListAffairTypes,
        pListAffairType
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
                            required, queryTable, restrict, (.==), (.<=), (.&&), (.<),
                            (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                            count, avg, sum, leftJoin, runQuery, showSqlForPostgres)

import Database.PostgreSQL.Simple.FromField (FromField(..))


-- type PlannerAffairType = (Integer,Integer,Integer,Maybe (PGArray Integer), String)

newtype BaseId = BaseId Int deriving (Show)

instance FromField BaseId where
  fromField field bs = BaseId <$> fromField field bs

instance QueryRunnerColumnDefault PGInt4 BaseId where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance QueryRunnerColumnDefault (Nullable PGText) [Char] where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

newtype ArInt = ArInt [Int]
{-
instance QueryRunnerColumnDefault PGInt4 ArInt where
  queryRunnerColumnDefault = fieldQueryRunnerColumn

instance FromField ArInt where
  fromField field bs = ArInt <$> fromField field bs
  -}
type PgIC = Column PGInt4
type MPgIC = Maybe PgIC
type AffairType = AffairTypePoly BaseId Int Int (PGArray PGInt4) String
type AffairTypeColumns = ( MPgIC, MPgIC, MPgIC, Column (Nullable (PGArray PGInt4)), Column (Nullable PGText))
type QueryAffairTypes = Query AffairTypePGR

data AffairTypePoly idn cd prio asc lbl = AffairType {idn ∷ idn, code ∷ cd, priority ∷ prio, activity_specific_codes ∷ asc, label ∷ lbl}
    deriving Show
type AffairTypePGW = AffairTypePoly MPgIC MPgIC MPgIC (Column (Nullable (PGArray PGInt4))) (Column (Nullable PGText))
type AffairTypePGR = AffairTypePoly PgIC PgIC PgIC (Column (Nullable (PGArray PGInt4))) (Column(Nullable PGText))
$(makeAdaptorAndInstance "pAffairType" ''AffairTypePoly)

-- Представления
type ListAffairType = ListAffairTypePoly BaseId Int String
type ListAffairTypeColumns = ( MPgIC, MPgIC, Column (Nullable PGText))
type QueryListAffairTypes = Query ListAffairTypePGR
data ListAffairTypePoly idn prio lbl = ListAffairType {_idn ∷ idn, _priority ∷ prio, _label ∷ lbl}
    deriving Show
type ListAffairTypePGW = ListAffairTypePoly MPgIC MPgIC (Column (Nullable PGText))
type ListAffairTypePGR = ListAffairTypePoly PgIC PgIC (Column(Nullable PGText))
$(makeAdaptorAndInstance "pListAffairType" ''ListAffairTypePoly)
