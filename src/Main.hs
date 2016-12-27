{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Frames
import Frames.CSV
import Pipes hiding (Proxy)
import qualified Pipes.Prelude as P

tableTypes "Row"  "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

rows :: Producer Row IO ()
rows = readTableOpt rowParser "data/U.S._Chronic_Disease_Indicators__CDI_.csv"

main :: IO ()
main = pipePreview rows 3 cat -- cat is the identity pipe and gets its name from the unix utility "cat"
