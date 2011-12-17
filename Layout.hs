
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout where

import Network
import Puzzle
import Space

import qualified Data.Set as Set

data Layout = Layout
  { lLines     :: [[Pos]]
  , lLineCount :: Int
  } deriving (Eq, Ord, Read, Show)

instance Puzzle Layout Network where
  solved net lay =
    nLineCount net == lLineCount lay
    && (Set.size $ lEdges $ head $ nLines net) == (length $ head $ lLines lay)
  choices net lay = [] -- TODO

