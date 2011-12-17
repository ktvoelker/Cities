
module Space where

data Pos = Pos
  { xPos :: Int
  , yPos :: Int
  } deriving (Eq, Ord, Read, Show)

data Vec = Vec
  { xDiff :: Int
  , yDiff :: Int
  } deriving (Eq, Ord, Read, Show)

distance :: Pos -> Pos -> Double
distance p1 p2 = sqrt $ fromIntegral $ toInteger $
  ((xPos p1 - xPos p2) ^ 2) + ((yPos p1 - yPos p2) ^ 2)

minus :: Pos -> Pos -> Vec
minus p1 p2 = Vec (xPos p1 - xPos p2) (yPos p1 - yPos p2)

plus :: Pos -> Vec -> Pos
plus p v = Pos (xPos p + xDiff v) (yPos p + yDiff v)

