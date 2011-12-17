
module Layout where

import Space

data Layout = Layout
  { lLines :: [[Pos]]
  } deriving (Eq, Ord, Read, Show)

