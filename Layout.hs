
{-# LANGUAGE MultiParamTypeClasses #-}
module Layout where

import Data.List

import Network
import Puzzle
import Space

data Layout = Layout
  { lLines    :: [[[Pos]]]  -- Completed lines
  , lRemLines :: [Line]     -- Lines not yet finished
  , lCurLine  :: [[Pos]]    -- Completed edges in the current line
  , lRemEdges :: [Edge]     -- Edges not yet finished in the current line
  , lCurEdge  :: [Pos]      -- The current edge in the current line
  } deriving (Eq, Ord, Read, Show)

initialLayout net = Layout
  { lLines     = []
  , lRemLines  = lines
  , lCurLine   = []
  , lRemEdges  = edges
  , lCurEdge   = [eFrom $ head edges]
  }
  where
    lines = nLines net
    edges = lEdges $ head lines

instance Puzzle Layout Network where
  solved net lay = null $ lRemLines lay
  choices net lay@Layout { lRemEdges = [] } =
    [Layout
      { lLines    = lCurLine lay : lLines lay
      , lRemLines = lines
      , lCurLine  = []
      , lRemEdges = edges
      , lCurEdge  = [eFrom $ head edges]
      }]
    where
      lines = tail $ lRemLines lay
      edges = lEdges $ head lines
  choices net lay
    | head (lCurEdge lay) == eTo (head $ lRemEdges lay) =
    [lay
      { lCurLine  = lCurEdge lay : lCurLine lay
      , lRemEdges = edges
      , lCurEdge  = [eFrom $ head edges]
      }]
    where
      edges = tail $ lRemEdges lay
  choices net lay =
    -- Extend layout with all remaining endpoints as choices.
    map ((\x -> lay { lCurEdge = x : lCurEdge lay }) . fst)
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
      curNetEdge = head $ lRemEdges lay
      curLayEdge = lCurEdge lay
      curDistance = distance (head curLayEdge) (eTo curNetEdge)

conflict :: Network -> Edge -> Pos -> Bool
conflict net edge pos = pos /= eTo edge && pos `elem` map nPos (nNodes net)

possibleExtensions :: [Pos] -> [Pos]
possibleExtensions (x : _) =
  [x `plus` Vec xd yd | xd <- [-1, 0, 1], yd <- [-1, 0, 1], xd /= 0 || yd /= 0]

