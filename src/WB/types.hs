module WB.Types where

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

newtype ObCode = ObCode String -- not assuming 2 digit

instance Show UTCtime where
  show (UTCtime y m d h mm) =
    show y ++ "-" ++ show m ++ "-" ++ show d ++
    "T" ++ show h ++ ":" ++ show mm
    
instance Show Position where
  show (Position x y) = show x ++ "," ++ show y
  
instance Show Temp where
  show (Temp t) = show t
  
instance Show ObCode where
  show (ObCode c) = show c

instance Show Record where
  show (Record u p t c) = show u ++ "|" ++
                          show p ++ "|" ++
                          show t ++ "|" ++
                          show c
