{-# LANGUAGE OverloadedStrings #-}
module WB.Generator where

import Control.Monad
import Test.QuickCheck

import System.IO
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


instance Variant a => Arbitrary a where
  arbitrary = oneof [valid, invalid]


instance Variant TRecord where
  valid = TRecord <$> printR <$> (arbitrary :: Gen Record)
  invalid = TRecord <$> arbitrary

instance Arbitrary TRecord where
  arbitrary = oneof [valid,invalid]

main = do
  let num = 1000000
  let config =
        [ ("ALL_VALID", "txt", num, valid)
        , ("RANDOM", "txt", num, arbitrary)
        ]
  mapM_ createTestSet config

createTestSet :: (Text, Text, Int, Gen TRecord) -> IO ()
createTestSet (fname, ext, count, gen) = do
  testSet <- generate $ vectorOf count gen
  mapM_ (writeToFile fname ext) (testSet)

writeToFile name_prefix suffix x = do
  TIO.appendFile (T.unpack $ T.intercalate "." [name_prefix, suffix]) (flip T.snoc '\n' $ textR x) 

