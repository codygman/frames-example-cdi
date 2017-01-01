{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Frames
import Frames.CSV
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P
import qualified Data.HashSet as HS
import qualified Data.Map as M
import Control.Lens
import Data.Vinyl.Core
import Frames.Diff

tableTypes "Row"  "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

rows :: Producer Row IO ()
rows = readTableOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

-- try to find record which isn't parsing
rows' :: Producer (ColFun Maybe Row) IO ()
rows' = readTableMaybeOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

rows'' :: Producer Row IO ()
rows'' = defaultingProducer "data/U.S._Chronic_Disease_Indicators__CDI_.csv" "rows'''"


-- how do I find the record before the one that turns out to be a Nothing?
-- you just filter records who evaluate to nothing after applying the recMaybe function:
-- pipePreview rows' 5 (P.filter (\r -> recMaybe r == Nothing))

-- writing CA location rows to file:
-- rows' <- inCoreAoS (rows >-> P.filter (\r -> rget locationAbbr r == "CA"))
-- λ> writeCSV  "/tmp/ca.csv" rows'
-- (0.09 secs, 235,231,984 bytes)


-- length of rows should be 237962 after subtracting header:
-- cody@zentop:~/source/frames-chronic-disease-indicators/src$ wc -l ../data/U.S._C
-- hronic_Disease_Indicators__CDI_.csv 
--    237962 ../data/U.S._Chronic_Disease_Indicators__CDI_.csv


distinctTopics rowProducer = do
  -- build a set by iterating over all rows and inserting topic values into a Set
  topics <- P.fold
              (\accumSet currentRow -> HS.insert (rget topic currentRow) accumSet) -- what we build up the set with
              HS.empty -- initial value
              id -- we just want to return the value we inserted, nothing more
              rowProducer -- our rowProducer, in this specific case it is just `rows`
  print topics

groupTopics :: ( Topic ∈ rs
               , LocationAbbr ∈ rs
               , Monad m) =>
               Producer (Record rs) m () -> m (M.Map Text (M.Map Text Integer))
groupTopics = P.fold
                (\accum currentRow ->
                    accum & at (rget locationAbbr currentRow) . non' _Empty -- if the locationAbbr isn't created, create with an empty map
                          . at (rget topic currentRow) . non 0 +~ 1         -- if topic for given locationAbbr doesn't exist, initialize at 0 and add 1
                )
                M.empty
                id

testInnerJoin = do
  joined <- innerJoin rows'' highConfidenceLimit rows'' highConfidenceLimit
  mapM_ (print . frameRow joined) [50..55]

biggestTopicsPerState = do
  grouped <- groupTopics rows
  mapM_ print $ M.toList (take 10 . reverse . M.toDescList <$> grouped)

main :: IO ()
main = groupTopics rows >>= print
