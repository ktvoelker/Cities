
module Puzzle where

import Control.Monad

class Puzzle a where
  solved :: a -> Bool
  choices :: a -> [a]

solve :: (Puzzle a, MonadPlus m) => a -> m a
solve p
  | solved p  = return p
  | otherwise = msum $ map solve $ choices p

explain :: (Puzzle a, MonadPlus m) => a -> m [a]
explain p = f [p]
  where
    f (p : ps)
      | solved p  = return $ p : ps
      | otherwise = msum $ map (f . (: (p : ps))) $ choices p

data Test = Test Int deriving (Show)

instance Puzzle Test where
  solved (Test 0) = True
  solved _ = False
  choices (Test n)
    | n <= 1    = []
    | otherwise = [Test $ n - 1, Test $ n - 2]

