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

tableTypes "Row"  "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

rows :: Producer Row IO ()
rows = readTableOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

distinctTopics rowProducer = do
  -- build a set by iterating over all rows and inserting topic values into a Set
  topics <- P.fold
              (\accumSet currentRow -> HS.insert (rget topic currentRow) accumSet) -- what we build up the set with
              HS.empty -- initial value
              id -- we just want to return the value we inserted, nothing more
              rowProducer -- our rowProducer, in this specific case it is just `rows`
  print topics

main :: IO ()
main = pipePreview rows 3 cat -- cat is the identity pipe and gets its name from the unix utility "cat"
