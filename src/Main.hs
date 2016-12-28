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

biggestTopicsPerState = do
  grouped <- groupTopics rows
  mapM_ print $ M.toList (take 10 . reverse . M.toDescList <$> grouped)

main :: IO ()
main = groupTopics rows >>= print
