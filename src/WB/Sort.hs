{-# LANGUAGE OverloadedStrings #-}
module WB.Sort where

import System.IO
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.List as L

import Data.Maybe

import WB.Types
import WB.Parse

getn file = do
  h <- openFile file ReadMode
  segmentFile file 0 h
  -- while thing

segmentFile :: String -> Int -> Handle -> IO a
segmentFile file rep h = do
  rs <- getNlines h
  let sorted = L.sort $ mapMaybe parseR rs
  w <- openFile ("partialSort_" ++ (show rep ++ "_") ++ file) WriteMode
  mapM_ ((TIO.hPutStrLn w).printR) sorted
  segmentFile file (rep+1) h
  
getNlines :: Handle -> IO [Text]
getNlines h = replicateM 1000000 (TIO.hGetLine h)
