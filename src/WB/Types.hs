{-# LANGUAGE OverloadedStrings #-}

module WB.Types where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Printf

data Record = Record {
  time :: UTCtime,
  pos :: Position,
  temp :: Temp,
  obs :: ObCode
  }
              
data UTCtime = UTCtime {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int
  }
               
data Position = Position {
  x :: Int,
  y :: Int
  }
                
newtype Temp = Temp Int

newtype ObCode = ObCode {
  ob :: Text
  }

instance Show UTCtime where
  show (UTCtime y m d h mm) =
    printf "%04d" y ++ "-" ++ printf "%02d" m
    ++ "-" ++ printf "%02d" d ++ "T" ++
    printf "%02d" h ++ ":" ++ printf "%02d" mm

instance Show Position where
  show (Position x y) = show x ++ "," ++ show y
  
instance Show Temp where
  show (Temp t) = show t
  
tshow :: Show a => a -> Text
tshow t = T.pack $ show t

printR :: Record -> Text
printR r = T.intercalate "|" [tshow $ time r, tshow $ pos r, tshow $ temp r, tshow $ ob $ obs r]
