{-# LANGUAGE DeriveGeneric, TemplateHaskell, UnicodeSyntax #-}
{-# LANGUAGE Arrows, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings #-}
module Database.Tables
    (
        listAffairTypesTable
    ) where

import GHC.Generics
import qualified Data.ByteString as BS (concat, ByteString)
import qualified Data.ByteString.Char8 as BSC8 (pack, unpack, unwords)
import Data.Profunctor.Product (p1,p2, p3, p6,p7)
import Data.Profunctor.Product.TH           (makeAdaptorAndInstance)
import Opaleye (Column, Nullable, matchNullable, isNull,
                          Table(Table), required, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                          count, avg, sum, leftJoin, optional, runQuery,
                          showSqlForPostgres, Unpackspec,PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)
import Database.Types (ListAffairType(..), ListAffairTypeColumns, ListAffairTypePGR, ListAffairTypePGW, ListAffairTypePoly(..), pListAffairType)

listAffairTypesTable ∷ Table ListAffairTypePGW ListAffairTypePGR
listAffairTypesTable = Table "list_affair_types" (pListAffairType ListAffairType {
                                        _idn = optional "id",
                                        _priority = optional "priority",
                                        _label = required "label"
                                        })
