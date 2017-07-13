{-# LANGUAGE OverloadedStrings, TemplateHaskell, QuasiQuotes, RecordWildCards, UnicodeSyntax #-}
module Main where
import Control.Monad.Trans.Either(EitherT(..), runEitherT, hoistEither, left)
import Control.Monad.IO.Class (liftIO)
import Data.Time
--import Data.Time.Clock

import Database.PostgreSQL.Simple as PGS(Connection, In (..), SqlError(..), connect,execute,execute_,query,query_)
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Types(PGArray)
-- import Yaml
import qualified Data.ByteString.Char8 as BSC8 (ByteString, putStrLn, pack, unpack)
-- import Data.ByteString.Char8
import AXConfigDB (readConfig, Config(..), db)
import Data.Typeable(typeOf)

import Control.Exception (catch)
-- import Database.Types(PlannerAffairType)
import Control.Lens ((^.))
import Database as DB (BaseId(..), ListAffairType(..), ListAffairTypePoly(..), getListAffairTypes)

import Opaleye  as OP(Column, Nullable, matchNullable, isNull,
                          Table(Table), required, queryTable,
                          Query, QueryArr, restrict, (.==), (.<=), (.&&), (.<),
                          (.===), (.++), ifThenElse, pgString, aggregate, groupBy,
                          count, avg, leftJoin, runQuery,
                          showSqlForPostgres, Unpackspec,PGUuid, PGInt4, PGInt8, PGText, PGDate, PGFloat8, PGBool)

{-
import Control.Monad (forM, forM_)
import Database.PostgreSQL.Simple.Errors
-}
-- TODO 5 INSERT INTO planner_affair_intervals VALUES (1,1,время начала,now())

{-
  3 585 find shortest path(haskell tasks
  4 959 exchange work
  7 1658 sport
-}
idn2 = 30001 ∷Int

{-
dangerous :: Connection → IO Int
dangerous c = do
  execute_ c [sql|INSERT INTO planner_affair_intervals VALUES (2,1,now(),now())|]

instance ToRow UTCTime where
  toRow = UTCTime <$> field
  -}

getDiff∷(Integer,UTCTime,UTCTime) → NominalDiffTime
getDiff (t,ts,te) = diffUTCTime te ts

{-
toTry ∷ Connection → IO ()
toTry c = do
  replicateM_ 3 $ execute_ c [sql|INSERT INTO planner_affair_types (priority) VALUES (0) |]
  -}
handler ∷ SqlError → IO ()
handler e = do
  BSC8.putStrLn . sqlErrorMsg $ e

type PlannerAffairType = (Integer,Integer,Integer,Maybe (PGArray Integer), String)

rz = "\t|\t"

showType ∷ PlannerAffairType → IO ()
showType (i, c, p, cs, l) = putStrLn $ (show i) ++ rz ++ (show p)++ rz ++ l

headTable = putStrLn $ "Номер" ++ rz ++ "Тип" ++ rz ++ "Метка"

showTypes ∷ [PlannerAffairType] → IO ()
showTypes ls = do
  headTable
  mapM_ showType ls

-- Новое отображение списка дел
showId (BaseId n) = show n
viewLineAffairType ListAffairType{..} = putStrLn $ (showId _idn) ++ rz ++ (show _priority)++ rz ++ _label

viewLinesAffairTypes ls = do
    headTable
    mapM_ viewLineAffairType ls

showListAffairTypes ∷ PGS.Connection → EitherT String IO [DB.ListAffairType]
showListAffairTypes conn = do
            d2 ← liftIO (catch ( do
                    d ← (runQuery conn DB.getListAffairTypes) ∷ IO [DB.ListAffairType]
--                    print d
                    return d)
                handler2)
            return d2

handler2 ∷ PGS.SqlError → IO [a]
handler2 e = do
  BSC8.putStrLn . PGS.sqlErrorMsg $ e
  return []

main ∷ IO ()
main = do
  cfg ← readConfig
  print cfg
  print $ cfg ^. db
  conn ← connect $ cfg ^. db
  catch (do
      -- mapM (\(x,y) → (putStrLn $ x) >> return (1,1)) =<<
--      res1 ← (query_ conn ([sql|SELECT * FROM planner_affair_types WHERE label != '' |] )) ∷ IO [PlannerAffairType]
--      showTypes res1
        ei <- runEitherT $ showListAffairTypes conn
        case ei of
                    Right x -> do
                        viewLinesAffairTypes x
                    Left m -> putStrLn m
        l ← getLine
        start ← getCurrentTime -- BSC8.pack .
        let ft = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
        print $ ft start
        res2 ← (query conn ([sql|SELECT id_affair_type,(time_start::timestamptz),(time_end::timestamptz) FROM planner_affair_intervals WHERE id_affair_type = ? AND time_start > '2016-08-22 00:00:00' ORDER BY time_start |] ) [(read l)∷Int] )∷ IO [(Integer,UTCTime,UTCTime)]
        putStrLn . unlines $ map show res2
        print . sum $ map getDiff res2
        getChar
        now ← getCurrentTime
        print $ ft now
        execute conn [sql|INSERT INTO planner_affair_intervals(id_affair_type,time_start,time_end) VALUES (?,?,?)|] [l,ft start,ft now] -- "2016-02-04 21:15"
        let dft = diffUTCTime now start
        print dft
        print $ (realToFrac dft)/3600
        return ())

    handler
