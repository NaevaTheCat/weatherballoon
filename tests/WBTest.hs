{-# LANGUAGE OverloadedStrings #-}
import Control.Monad
import Control.Monad.IO.Class
import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.IO
import System.Directory

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import WB.Types
import WB.Parse
import WB.Generator
import WB.Sort
import WB.Stats

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

prop_meanStable =
  let g = choose (1, 10000) :: Gen Int in
  forAll g $ \num -> monadicIO $ do
    liftIO $ createTestSet ("QuickCheck", "txt", num, valid)
    sortedTF <- liftIO $ mergeSort "QuickCheck.txt" 1000 1000
    h <- liftIO $ openFile "QuickCheck.txt" ReadMode
    h' <- liftIO $ openFile sortedTF ReadMode
    mean <- liftIO $ meanTemp h 0 0
    mean' <- liftIO $ meanTemp h' 0 0
    liftIO $ mapM removeFile ["QuickCheck.txt", sortedTF]
    return $ mean === mean'
    
prop_saneStats =
  let g = choose (1, 10000) :: Gen Int in
  forAll g $ \num -> monadicIO $ do
    liftIO $ createTestSet ("QuickCheck", "txt", num, valid)
    h <- liftIO $ openFile "QuickCheck.txt" ReadMode
    minT <- liftIO $ minTemp h (Temp 0) True
    h <- liftIO $ openFile "QuickCheck.txt" ReadMode
    maxT <- liftIO $ maxTemp h (Temp 0) True
    h <- liftIO $ openFile "QuickCheck.txt" ReadMode
    meanT <- liftIO $ meanTemp h 0 0
    liftIO $ removeFile "QuickCheck.txt"
    assert $ (fromIntegral $ t minT) <= meanT
    assert $ (fromIntegral $ t maxT) >= meanT

prop_AtoBBtoA =
  let g = choose (2, 10000) :: Gen Int in
  forAll g $ \num -> monadicIO $ do
    liftIO $ createTestSet ("QuickCheck", "txt", num, valid)
    sortedTF <- liftIO $ mergeSort "QuickCheck.txt" 10000 10000
    fileconts <- liftIO $ TIO.readFile sortedTF
    liftIO $ TIO.writeFile "Flipped.txt" fileconts
    h <- liftIO $ openFile sortedTF ReadMode
    h' <- liftIO $ openFile "Flipped.txt" ReadMode
    d1 <- liftIO $ totalDist h (Position 0 0) 0 True
    d2 <- liftIO $ totalDist h' (Position 0 0) 0 True
    liftIO $ mapM_ removeFile ["Flipped.txt","QuickCheck.txt",sortedTF]
    pure $ d1 === d2
    
-- full coverage of conversion functions, avoiding the discard KM with Z+ restriction
prop_auConvIdem r = (r === (mToKm $ kToC $ auToSI r))
prop_usConvIdem r = (r === (mToMiles $ kToF $ usToSI r))
                        
main :: IO ()
main = do
  putStrLn "Parser accepts"
  quickCheck prop_idempotent
  putStrLn "Parser rejects"
  quickCheck prop_rejects
  putStrLn "au to si and back"
  quickCheck prop_auConvIdem
  putStrLn "us to si and back"
  quickCheck prop_usConvIdem
  putStrLn "min <= mean <= max"
  quickCheck prop_saneStats
  putStrLn "Mean is stable across sorts"
  quickCheck prop_meanStable
  putStrLn "Distance works both ways"
  quickCheck prop_AtoBBtoA
  putStrLn "Sort is stable"
  quickCheck prop_sortIdempotent
