{-# LANGUAGE Arrows #-}
{-# LANGUAGE DeriveGeneric, UnicodeSyntax #-}
module Database.Queries
    (
    getListAffairTypes
    ) where
import GHC.Generics
import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Database.Types (ListAffairTypePoly(..), QueryListAffairTypes)
import Database.Tables (listAffairTypesTable)
import Control.Arrow (returnA, (<<<))
import Opaleye as OP((.==), (./=), (.<=), (.&&), (.<), (.===), (.++),
    Column, Nullable, PGBool(..), PGDate, PGFloat8, PGInt4, PGInt8, PGText, PGUuid, Table(Table), Query, QueryArr, Unpackspec,
    aggregate, avg, count, groupBy, isNull, matchNullable, not, required, queryTable, restrict, ifThenElse, pgString, pgStrictText, sum, leftJoin, runQuery, showSqlForPostgres)

{-
getAffairTypes ∷ QueryAffairTypes
getAffairTypes = proc () -> do
    affairTypes@AffairType {label = pgLabel} <- queryTable affairTypesTable -< ()
--    af <- queryTable affairTypesTable -< ()
    restrict -< ( isNull pgLabel) --  ./= (pgString "")
    returnA -< affairTypes
    -}
    
getListAffairTypes ∷ QueryListAffairTypes
getListAffairTypes = proc () -> do
--    listAffairTypes@ListAffairType {label' = pgLabel} <- queryTable listAffairTypesTable -< ()
    af <- queryTable listAffairTypesTable -< ()
--    restrict -< ( OP.not $ isNull pgLabel ) --  ./= (pgString "")
    returnA -< af
