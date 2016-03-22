{-# LANGUAGE OverloadedStrings #-}
module WB.Sort where

import System.IO
import System.IO.Error
import System.Directory

import Data.Either 
import Control.Monad

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Ord
import Data.List as L

import Data.Maybe

import WB.Types
import WB.Parse

data Flag = Odd | Even deriving(Eq, Show)

flipFlag :: Flag -> Flag
flipFlag Odd = Even
flipFlag Even = Odd

mergeSort :: String -> Int -> Int -> IO String
mergeSort file segmentSize mergeSize= do
  h <- openFile file ReadMode
  reps <- segmentFile file 0 h segmentSize
  outName <- mergeAll file reps 0 mergeSize 0 Even
  renameFile outName ("Sorted_" ++ file)
  cleanUp file reps
  return $ "Sorted_" ++ file


mergeAll :: String -> Int -> Int -> Int -> Int -> Flag -> IO String
mergeAll file num pos n rep flag
  | (pos == num) && (rep > 0)= return $ mergeName $ flipFlag flag
  | rep > 0 = do
    let (hs', pos') = openNorLess file num pos n
    hs <- hs'
    h <- openFile (mergeName $ flipFlag flag) ReadMode
    let toMerge = h : hs
    w <- openFile (mergeName flag) WriteMode
    rs' <- mapM TIO.hGetLine toMerge
    let rs = mapMaybe parseR rs' -- only valid records are in the merge file
    mergeSome toMerge rs w
    mergeAll file num pos' n (rep+1) (flipFlag flag)
    
  | otherwise = do
    let (hs', pos') = openNorLess file num pos n
    hs <- hs'
    w <- openFile (mergeName flag) WriteMode
    rs' <- mapM TIO.hGetLine hs
    let rs = mapMaybe parseR rs'
    mergeSome hs rs w
    mergeAll file num pos' n (rep+1) (flipFlag flag) where
      mergeName f = "merge_" ++ show f
    
-- Incrementally write the minimum then tick that handle through
mergeSome :: [Handle] -> [Record] -> Handle -> IO ()
mergeSome [] _ w = do
  hClose w
  return ()
mergeSome hs rs w = do
  let (ind, r) = mindex rs
  let rs' = (take ind rs) ++ (drop (ind + 1) rs)
  TIO.hPutStrLn w $ printR r
  (hs', mRec) <- readNextofI hs ind
  case mRec of
    Nothing -> mergeSome hs' rs' w
    _ -> mergeSome hs' ((fromJust mRec):rs') w
  
-- Reads the next entry in the Ith handle. On EOF error sheds that from list
readNextofI :: [Handle]-> Int -> IO ([Handle], Maybe Record)
readNextofI hs ind = do
  let first = take ind hs
  let focus = head $ drop ind hs
  let rest = drop (ind + 1) hs
  r <- safeRead focus
  case r of
    Nothing -> do
               hClose focus
               return $ (first ++ rest, Nothing)
    Just x  -> return $ (focus : first ++ rest, Just x)

safeRead :: Handle -> IO (Maybe Record)
safeRead h = catchIOError (parseR <$> TIO.hGetLine h) catcher where
  catcher e = if isEOFError e then return Nothing else ioError e

mindex :: [Record] -> (Int, Record)
mindex xs = L.minimumBy (comparing snd) $ zip [0..] xs

-- Opens the partial sorts, returns the handles and position
openNorLess :: String -> Int -> Int -> Int -> (IO [Handle], Int)
openNorLess file num pos n
  | (num - pos) < n = (mapM (openfunc file) [pos.. num], num)
  | otherwise = (mapM (openfunc file) [pos.. (pos+n)], pos + num) where
    openfunc file x = openFile ("partialSort_" ++ (show x ++ "_") ++ file) ReadMode

-- Sorts parts of a file
segmentFile :: String -> Int -> Handle -> Int-> IO Int
segmentFile file rep h num = do
  rs <- getNlines num h
  case rs of
    [] -> return $ rep -1 -- We hit EOF last run
    _ -> do
      let sorted = L.sort $ mapMaybe parseR rs
      w <- openFile ("partialSort_" ++ (show rep ++ "_") ++ file) WriteMode
      mapM_ ((TIO.hPutStrLn w).printR) sorted
      hClose w
      segmentFile file (rep+1) h num

-- Gets N lines, returns shortened list on EOF
-- returns [] if called at the EOF
getNlines :: Int -> Handle -> IO [Text]
getNlines 0  _ = return []
getNlines n h = let catch e = if isEOFError e then return $ Left "" else ioError e in
  do
    t <- (catchIOError (Right <$> TIO.hGetLine h) catch )
    case t of
      Right t' -> do
        foo <- getNlines (n-1) h
        pure $ t' : foo
      Left t' -> return []
  
cleanUp :: String -> Int -> IO ()
cleanUp file reps = mapM_ delete [0..reps] where
  delete rep = removeFile $ "partialSort_" ++ (show rep ++ "_") ++ file
