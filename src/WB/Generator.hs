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

gen file num validOnly = do
  let generator = if validOnly == True then valid else arbitrary
  let config =
        (file, "txt", num, generator)
  createTestSet config

createTestSet :: (String, String, Int, Gen TRecord) -> IO ()
createTestSet (fname, ext, count, gen) = do
  testSet <- generate $ vectorOf count gen
  h <- openFile (fname ++ "." ++ ext) WriteMode
  mapM_ (writeToFile fname ext h) testSet
  hClose h

writeToFile name suffix h x = TIO.hPutStrLn h (textR x)
