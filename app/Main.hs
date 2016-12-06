{-# LANGUAGE TemplateHaskell, QuasiQuotes, UnicodeSyntax #-}
module Main where
import Data.Time
--import Data.Time.Clock

import Database.PostgreSQL.Simple(connect,execute,execute_,query,query_, In (..), SqlError(..))
import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple.Time
import Database.PostgreSQL.Simple.Types ( PGArray)
-- import Yaml
import qualified Data.ByteString.Char8 as BSC8 (ByteString, putStrLn, pack, unpack)
-- import Data.ByteString.Char8
import AXConfigDB (readConfig, Config(..))
import Data.Typeable(typeOf)

import Control.Exception (catch)

type PlannerAffairType = (Integer,Integer,Integer,Maybe (PGArray Integer), String)
{-import Control.Monad (forM, forM_)
import Database.PostgreSQL.Simple.Errors
-}
-- TODO 5 INSERT INTO planner_affair_intervals VALUES (1,1,время начала,now())

{-
  3 585 find shortest path(haskell tasks
  4 959 exchange work
  7 1658 sport
-}
idn2 = 30001 ∷Int
rz = "\t|\t"
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

showType ∷ PlannerAffairType → IO ()
showType (i, c, p, cs, l) = putStrLn $ (show i) ++ rz ++ (show p)++ rz ++ l

showTypes ∷ [PlannerAffairType] → IO ()
showTypes ls = do
  putStrLn $ "Номер" ++ rz ++ "Тип" ++ rz ++ "Метка"
  mapM_ showType ls

-- label != ''
-- code > 5000000 AND code < 6000000
main ∷ IO ()
main = do
  cfg ← readConfig
  print cfg
  print $ db cfg
  conn ← connect $ db cfg
  catch (do
      -- mapM (\(x,y) → (putStrLn $ x) >> return (1,1)) =<< 
      res1 ← (query_ conn ([sql|SELECT * FROM planner_affair_types |] )) ∷ IO [PlannerAffairType]
      showTypes res1
      l ← getLine
      start ← getCurrentTime -- BSC8.pack .
      let ft =  formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S"
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
