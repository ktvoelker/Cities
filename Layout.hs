
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout where

import Network
import Puzzle
import Space

data Layout = Layout
  { lLines     :: [[[Pos]]]
  , lLineCount :: Int
  } deriving (Eq, Ord, Read, Show)

initialLayout net = Layout [[[eFrom $ last $ lEdges $ last $ nLines net]]] 1

instance Puzzle Layout Network where
  solved net lay =
    nLineCount net == lLineCount lay
    && (length $ lEdges $ head $ nLines net) == (length $ head $ lLines lay)
  choices net lay =
    if lenCurNetLine == lenCurLayLine
    then -- We've finished this line, so go on to the next.
      choices net Layout { lLines = [[]] : lLines lay, lLineCount = lLineCount lay + 1 }
    else -- We're in the middle of this line.
      if eTo curNetEdge == head curLayEdge
      then -- We've finished this edge, so go on to the next.
        choices net lay { lLines = ([] : curLayLine) : tail (lLines lay) }
      else -- We're in the middle of this edge.
        [] -- TODO remove
        -- Extend layout with all remaining endpoints as choices.
        -- TODO
        -- Filter out endpoints further from the goal than the old endpoint.
        -- TODO
        -- Sort the new endpoints by distance from the goal.
        -- TODO
        -- Find all possible extensions of the partial edge.
        -- TODO
    where
      curNetLine = lEdges $ head $ drop (nLineCount net - lLineCount lay) $ nLines net
      curLayLine = head $ lLines lay
      lenCurNetLine = length curNetLine
      lenCurLayLine = length curLayLine
      curNetEdge = head curNetLine
      curLayEdge = head curLayLine

