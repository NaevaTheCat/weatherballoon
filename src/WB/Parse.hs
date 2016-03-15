{-# LANGUAGE OverloadedStrings #-}

module WB.Parse where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Attoparsec.Text
import Control.Applicative

import WB.Types

parseUTC :: Parser UTCtime
parseUTC = do
  y <- count 4 digit -- good for ~8000 years
  char '-'
  m <- count 2 digit
  char '-'
  d <- count 2 digit
  char 'T'
  h <- count 2 digit
  char ':'
  mm <- count 2 digit
  return $ UTCtime (read y) (read m) (read d) (read h) (read mm)

parsePos :: Parser Position
parsePos = do
  x <- decimal
  char ','
  y <- decimal
  return $ Position x y

parseTemp :: Parser Temp
parseTemp = do
  t <- signed decimal
  return $ Temp t

parseOb :: Parser ObCode
parseOb = do
  o <- many1 letter
  endOfInput
  return $ ObCode $ T.pack o

parseRecord :: Parser Record
parseRecord = do
  u <- parseUTC
  char '|'
  p <- parsePos
  char '|'
  t <- parseTemp
  char '|'
  o <- parseOb
  return $ Record u p t o
<<<<<<< HEAD

parseR :: Text -> Maybe Record
parseR r =
  case (parseOnly parseRecord r) of
    Left _ -> Nothing
    Right a -> Just a
=======
>>>>>>> Added for records and converting of records to formatted text
