{-# LANGUAGE OverloadedStrings #-}
module WB.Stats where

import System.IO
import System.Directory

import qualified Data.Text.IO as TIO

import Data.Maybe
import qualified Data.List as L
import WB.Types
import WB.Sort (safeRead)
--AU C km
--US f miles
--FR K m
--others K km

-- rounding to nearest whole unit, because data
toSI :: Record -> Record
toSI r = case (obs r) of
  ObCode "AU" -> auToSI r
  ObCode "US" -> usToSI r
  ObCode "FR" -> r -- highly sensible
  _ -> otherToSI r

kToC r = Record (time r) (pos r) newTemp (obs r) where
  newTemp = Temp $ (t $ temp r) - 273
  
kToF r = Record (time r) (pos r) newTemp (obs r) where
  newTemp = Temp $ round $ ((fromIntegral $ t $ temp r) - 273.15) * 1.8 + 32
  
mToMiles r = Record (time r) newPos (temp r) (obs r) where
  newPos = Position (conv $ x $ pos r) (conv $ y $ pos r)
  conv x = round $ (fromIntegral x) * 0.00062137

mToKm r = Record (time r) newPos (temp r) (obs r) where
  newPos = Position (conv $ x $ pos r) (conv $ y $ pos r)
  conv x = round $ (fromIntegral x) / 1000
  
auToSI r = Record (time r) newPos newTemp (obs r) where
  newPos = Position ((x $ pos r) * 1000) ((y $ pos r) * 1000)
  newTemp = Temp $ (t $ temp r) + 273
  
usToSI r = Record (time r) newPos newTemp (obs r) where
  newPos = Position (conv $ x $ pos r) (conv $ y $ pos r)
  conv x = round $ (fromIntegral x)/0.00062137
  newTemp = Temp $ round $((fromIntegral $ t $ temp r) - 32 )/1.8 + 273.15
  
otherToSI r = Record (time r) newPos (temp r) (obs r) where
  newPos = Position (conv $ x $ pos r) (conv $ y $ pos r)
  conv x = round $ (fromIntegral x) / 1000 -- really throwing these away but making up accuracy (limited to Z+ from example) doesn't sit well with me

maxTemp :: Handle -> Temp -> Bool -> IO Temp
maxTemp h _ True = do
  r <- safeRead h
  case r of
    Nothing -> undefined -- error here
    Just r' -> maxTemp h (temp $ toSI r') False
maxTemp h t firstRun = do
  r <- safeRead h
  case r of
    Nothing -> do
      hClose h
      return t
    Just r' -> case (compare t (temp $ toSI r')) of
      LT -> maxTemp h (temp $ toSI r') False
      _ -> maxTemp h t False
      
minTemp :: Handle -> Temp -> Bool -> IO Temp
minTemp h _ True = do
  r <- safeRead h
  case r of
    Nothing -> undefined -- error here
    Just r' -> minTemp h (temp $ toSI r') False
minTemp h t firstRun = do
  r <- safeRead h
  case r of
    Nothing -> do
      hClose h
      return t
    Just r' -> case (compare t (temp $ toSI r')) of
      GT -> minTemp h (temp $ toSI r') False
      _ -> minTemp h t False

meanTemp :: Handle -> Double -> Double -> IO Double
meanTemp h lastMean k = do
  r <- safeRead h
  case r of
    Nothing -> do
      hClose h
      return lastMean
    Just r' -> meanTemp h mean (k + 1) where
      mean = lastMean + (t_k - lastMean)/(k + 1)
      t_k = fromIntegral $ t $ temp $ toSI r'

countObs :: Handle -> [(ObCode, Int)] -> IO [(ObCode, Int)]
countObs h seen = do
  r <- safeRead h
  case r of
    Nothing -> do
      hClose h
      return seen
    Just r' -> countObs h seen' where
      seen' = case (lookup (obs r') seen) of
        Nothing -> (obs r', 1) : seen
        Just i ->  map (\(a,b) -> 
                         if a == (obs r')
                         then (a, b+1)
                         else (a,b)) seen

totalDist :: Handle -> Position -> Double -> Bool -> IO Double
totalDist h _ _ True = do
  r <- safeRead h
  case r of
    Nothing -> undefined -- error here
    Just r' -> totalDist h (pos r') 0 False
totalDist h p dist firstRun = do
  r <- safeRead h
  case r of
    Nothing -> do
      hClose h
      return dist
    Just r' -> totalDist h (pos r') newdist False where 
      newdist = calcDist p (pos r') 

calcDist :: Position -> Position -> Double
calcDist (Position x1 y1) (Position x2 y2) =
  sqrt (((fromIntegral x2) - (fromIntegral x1))^2
        + ((fromIntegral y2) - fromIntegral y1)^2)

converter :: String -> ((Record -> Record),(Record -> Record)) -> IO ()
converter fname (d,t) = do
  h <- openFile fname ReadMode
  w <- openFile "temp_conv" WriteMode
  mapConv h w d
  hClose h
  hClose w
  h' <- openFile "temp_conv" ReadMode
  w' <- openFile fname WriteMode
  mapConv h w t
  hClose h'
  hClose w'
  removeFile "temp_conv"

mapConv :: Handle -> Handle -> (Record -> Record) -> IO ()
mapConv h w f = do
  r <- safeRead h
  case r of
    Nothing -> return ()
    Just r' -> do
      let out = f $ toSI r'
      TIO.hPutStrLn w $ printR out
      mapConv h w f
