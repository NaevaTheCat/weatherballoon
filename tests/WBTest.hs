{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import WB.Types
import WB.Parse
import WB.Generator
import WB.Sort

prop_idempotent r = (parseR $ printR r) === Just r
prop_rejects r = (parseR r) === Nothing --vanishingly small chance we'll generate a valid input

-- make a test file of valid inputs of random length,
-- sort that, sort the results,
-- compare that line by line to make sure it's the same
prop_sortIdempotent =
  let g = choose (1,10000) :: Gen Int in 
  forAll ((,,)
  <$> g
  <*> g
  <*> g)
  $ \(num, segSize, merSize) ->  monadicIO $ do
    liftIO $ createTestSet ("QuickCheck", "txt", num, valid)
    sortedTF <- liftIO $ mergeSort "QuickCheck.txt" segSize merSize
    sortedTF' <- liftIO $ mergeSort sortedTF segSize merSize
    result <- liftIO $ compare2Files sortedTF sortedTF'
    assert $ result where
      compare2Files f1 f2 = do
        cont1 <- TIO.readFile f1
        cont2 <- TIO.readFile f2
        mapM removeFile ["QuickCheck.txt", f1, f2]
        return $ cont1 == cont2

main :: IO ()
main = do
  quickCheck prop_idempotent
  quickCheck prop_rejects
  quickCheck prop_sortIdempotent
