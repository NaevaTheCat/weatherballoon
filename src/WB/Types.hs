{-# LANGUAGE OverloadedStrings #-}

module WB.Types where

import Data.Text (Text)
import qualified Data.Text as T

import Text.Printf

import Test.QuickCheck

import Control.Applicative

data Record = Record {
  time :: UTCtime,
  pos :: Position,
  temp :: Temp,
  obs :: ObCode
  } deriving Eq
              
data UTCtime = UTCtime {
  year :: Int,
  month :: Int,
  day :: Int,
  hour :: Int,
  minute :: Int
  } deriving Eq
               
data Position = Position {
  x :: Int,
  y :: Int
  } deriving Eq
                
newtype Temp = Temp {
  t :: Int
  } deriving(Eq, Ord)

newtype ObCode = ObCode {
  ob :: Text
  } deriving Eq

instance Show UTCtime where
  show (UTCtime y m d h mm) =
    printf "%04d" y ++ "-" ++ printf "%02d" m
    ++ "-" ++ printf "%02d" d ++ "T" ++
    printf "%02d" h ++ ":" ++ printf "%02d" mm

instance Show Position where
  show (Position x y) = show x ++ "," ++ show y
  
instance Show Temp where
  show (Temp t) = show t

instance Show Record where
  show (Record u p t c) = show u ++ "|" ++
                          show p ++ "|" ++
                          show t ++ "|" ++
                          T.unpack (ob c)
  
tshow :: Show a => a -> Text
tshow t = T.pack $ show t

printR :: Record -> Text
printR r = T.intercalate "|" [tshow $ time r, tshow $ pos r, tshow $ temp r, ob $ obs r]

instance Ord UTCtime where
  compare (UTCtime y m d h mm) (UTCtime y' m' d' h' mm') =
    comparify [y,m,d,h,mm] [y',m',d',h',mm']

-- purely a helper for avoiding stair stepping in UTC compare
comparify :: [Int] -> [Int] -> Ordering
comparify [] [] = EQ
comparify (a:as) (b:bs) = case (compare a b) of
  LT -> LT
  GT -> GT
  EQ -> comparify as bs

instance Ord Record where
  compare a b = compare (time a) (time b)

-- Arbitraries
instance Arbitrary UTCtime where
  arbitrary = UTCtime <$> year <*> month <*> day
              <*> hour <*> minute where
                year = choose (0, 9999)
                month = choose (1, 12)
                day = choose (1, 31)
                hour = choose (0, 23)
                minute = choose (0, 59)
instance Arbitrary Position where
  arbitrary = Position <$> pos <*> pos where
    pos = getPositive <$> arbitrary
instance Arbitrary Temp where
  arbitrary = Temp <$> arbitrarySizedIntegral
instance Arbitrary ObCode where
  arbitrary = ObCode <$> oneof [knownobs, randobs]

knownobs :: Gen Text
knownobs = elements ["AU","US","FR"]

-- Assume capital letter restriction on ObCodes
randobs :: Gen Text
randobs = T.pack <$> listOf1 textcode where
  textcode = elements ['A'..'Z']

instance Arbitrary Record where
  arbitrary = Record <$>
              arbitrary <*>
              arbitrary <*>
              arbitrary <*>
              arbitrary
              
instance Arbitrary Text where
  arbitrary = T.pack <$> listOf1 textChar where
    textChar = elements . concat $ [ ['a'..'z']
                                   , ['A'..'Z']
                                   , ['0'..'9']
                                   , [' '..'/']
                                   ]
