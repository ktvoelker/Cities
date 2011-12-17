
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
  choices net lay =
    if Set.size curNetLine == length curLayLine
    then -- We've finished this line, so go on to the next.
      choices net Layout { lLines = [] : lLines lay, lLineCount = lLineCount lay + 1 }
    else -- We're in the middle of this line, so place the next edge.
      [] -- TODO
    where
      curNetLine = lEdges $ head $ drop (nLineCount net - lLineCount lay) $ nLines net
      curLayLine = head $ lLines lay

