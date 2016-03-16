{-# LANGUAGE OverloadedStrings #-}
module WB.Generator where

import Control.Monad
import Test.QuickCheck

import System.IO
import System.Environment
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import WB.Types

newtype TRecord = TRecord {
  textR :: Text
  }
                  
class Variant a where
  valid :: Gen a
  invalid :: Gen a

instance Variant TRecord where
  valid = TRecord <$> printR <$> (arbitrary :: Gen Record)
  invalid = TRecord <$> arbitrary

instance Arbitrary TRecord where
  arbitrary = oneof [valid,invalid]

gen = do
--  args <- getArgs
  num <- (read . head) <$> getArgs
  let config =
        [ ("ALL_VALID", "txt", num, valid)
        , ("RANDOM", "txt", num, arbitrary)
        ]
  mapM_ createTestSet config

createTestSet :: (Text, Text, Int, Gen TRecord) -> IO ()
createTestSet (fname, ext, count, gen) = do
  testSet <- generate $ vectorOf count gen
  h <- openFile (T.unpack $ T.intercalate "." [fname, ext]) WriteMode
  mapM_ (writeToFile fname ext h) testSet

writeToFile name suffix h x = TIO.hPutStrLn h (textR x)
