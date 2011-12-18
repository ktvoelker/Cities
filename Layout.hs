
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout where

import Data.List

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
        -- Extend layout with all remaining endpoints as choices.
        map ((\x -> lay { lLines =
          ((x : curLayEdge) : tail curLayLine) : tail (lLines lay) }) . fst)
        -- Filter out endpoints further from the goal than the old endpoint.
        $ filter ((<= curDistance) . snd)
        -- Sort the new endpoints by distance from the goal.
        $ sortBy (\x y -> compare (snd x) (snd y))
        $ map (\x -> (x, distance x (eTo curNetEdge)))
        -- Filter out endpoints that are in conflict with a node.
        $ filter (not . conflict net curNetEdge)
        -- Find all possible extensions of the partial edge.
        $ possibleExtensions curLayEdge
    where
      curNetLine = lEdges $ head $ drop (nLineCount net - lLineCount lay) $ nLines net
      curLayLine = head $ lLines lay
      lenCurNetLine = length curNetLine
      lenCurLayLine = length curLayLine
      curNetEdge = head curNetLine
      curLayEdge = head curLayLine
      curDistance = distance (head curLayEdge) (eTo curNetEdge)

conflict :: Network -> Edge -> Pos -> Bool
conflict net edge pos = pos /= eTo edge && pos `elem` map nPos (nNodes net)

possibleExtensions :: [Pos] -> [Pos]
possibleExtensions (x : _) =
  [x `plus` Vec xd yd | xd <- [-1, 0, 1], yd <- [-1, 0, 1], xd /= 0 || yd /= 0]

