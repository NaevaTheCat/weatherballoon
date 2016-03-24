{-# LANGUAGE OverloadedStrings #-}
module Main where

import Options.Applicative

import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import System.IO
import Control.Monad

import WB.Types
import WB.Generator
import WB.Sort
import WB.Stats
import WB.Parse

type File = String
type Number = Int
type ValidOnly = Bool
type Target = String
type Dist = String
type Degrees = String
type Min = Bool
type Max = Bool
type Mean = Bool
type NObs = Bool
type TDist = Bool

data Command
  = Generate Number ValidOnly
  | Analyse Dist Degrees Min Max Mean TDist NObs

data Options = Options Target Command
main :: IO ()
main = run =<< execParser
       (parseOptions `withInfo` "WeatherBallon generate or analyse files!")

parseOptions :: Parser Options
parseOptions = Options <$> parseTarget <*> parseCommand

parseTarget :: Parser Target
parseTarget = strOption $
  short 't' <> long "target" <> metavar "OUTPUT-FILE" <>
  help "Designate output file, file will be overwritten if already exists"

parseCommand :: Parser Command
parseCommand = subparser $
  command "generate" (parseGenerate `withInfo` "Generate a test file") <>
  command "analyse" (parseAnalyse `withInfo` "Analyse and sort file")

parseGenerate :: Parser Command
parseGenerate = Generate
  <$> argument auto (metavar "N(Int)")
  <*> argument auto (metavar "ValidOnly(True|False)")

parseAnalyse :: Parser Command
parseAnalyse = Analyse
  <$> argument distarg (metavar "km|mi|m")
  <*> argument temparg (metavar "C|f|K")
  <*> argument auto (metavar "Min(True|False)")
  <*> argument auto (metavar "Max(True|False)")
  <*> argument auto (metavar "Mean(True|False)")
  <*> argument auto (metavar "Distance(True|False)")
  <*> argument auto (metavar "NObs(True|False)")

distarg :: ReadM String
distarg = eitherReader $ \arg -> case arg of
  "km" -> return "km"
  "mi" -> return "mi"
  "m" -> return "m"
  _ -> Left $ "cannot parse value `" ++ arg ++ "'"
  
temparg :: ReadM String
temparg = eitherReader $ \arg -> case arg of
  "C" -> return "C"
  "f" -> return "f"
  "K" -> return "K"
  _ -> Left $ "cannot parse value `" ++ arg ++ "'"
  
withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

run :: Options -> IO ()
run (Options target cmd) = do
  case cmd of
    Generate num validOnly -> gen target num validOnly
    Analyse dist deg mi ma me totd oc -> doAnalysis target dist deg mi ma me totd oc

doAnalysis target di de mi ma me td oc = do
  out <- mergeSort target 10000 10000
  converter out $ unitHappiness (di,de)
  w <- openFile (statsOut target) WriteMode
  when mi $ do
    h <- openFile out ReadMode
    minT <- minTemp h (Temp 0) True
    TIO.hPutStrLn w $ T.pack $ "Minimum Temperature " ++ de ++ ":"
    TIO.hPutStrLn w $ T.pack $ show minT
  when ma $ do
    h <- openFile out ReadMode
    maxT <- maxTemp h (Temp 0) True
    TIO.hPutStrLn w $ T.pack $ "Maximum Temperature " ++ de ++ ":"
    TIO.hPutStrLn w $ T.pack $ show maxT
  when me $ do
    h <- openFile out ReadMode
    meanT <- meanTemp h 0 0
    TIO.hPutStrLn w $ T.pack $ "Mean Temperature " ++ de ++ ":"
    TIO.hPutStrLn w $ T.pack $ show meanT
  when td $ do
    h <- openFile out ReadMode
    totalD <- totalDist h (Position 0 0) 0 True
    TIO.hPutStrLn w $ T.pack $ "Total Distance " ++ di ++ ":"
    TIO.hPutStrLn w $ T.pack $ show totalD
  when oc $ do
    h <- openFile out ReadMode
    obsCount <- countObs h []
    TIO.hPutStrLn w "Observatory Counts:"
    TIO.hPutStrLn w $ T.pack $ show obsCount

statsOut :: String -> String
statsOut target = "Summary_" ++ target

unitHappiness :: (String, String) -> ((Record -> Record),(Record -> Record))
unitHappiness (d,t) = (df d, tf t) where
  df "km" = mToKm
  df "mi" = mToMiles
  df "m"  = id
  tf "C"  = kToC
  tf "f"  = kToF
  tf "K"  = id
