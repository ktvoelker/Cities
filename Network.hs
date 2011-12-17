
module Network where

import qualified Data.Set as Set

import Space

data Node = Node
  { nPos    :: Pos
  , nName   :: String
  } deriving (Eq, Ord, Read, Show)

data Edge = Edge
  { eFrom :: Pos
  , eTo   :: Pos
  } deriving (Eq, Ord, Read, Show)

data Symbol = Symbol
  { sColor :: Color
  , sText  :: String
  , sShape :: Shape
  } deriving (Eq, Ord, Read, Show)

data Shape = Circle | Square | Diamond | Hexagon | UpTriangle | DownTriangle
  deriving (Enum, Eq, Ord, Read, Show)

data Color = Color
  { cRed   :: Int
  , cGreen :: Int
  , cBlue  :: Int
  } deriving (Eq, Ord, Read, Show)

data Line = Line
  { lEdges  :: Set.Set Edge
  , lName   :: String
  , lSymbol :: Symbol
  , lColor  :: Color
  } deriving (Eq, Ord, Read, Show)

data Network = Network
  { nNodes     :: Set.Set Node
  , nLines     :: [Line]
  , nLineCount :: Int
  } deriving (Eq, Ord, Read, Show)

