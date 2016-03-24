module Main where

import Options.Applicative

import WB.Types
import WB.Generator
import WB.Sort
import WB.Stats
import WB.Parse

main :: IO ()
main = gen
