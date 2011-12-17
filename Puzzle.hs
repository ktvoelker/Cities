
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Puzzle where

import Control.Monad

class Puzzle a b | a -> b where
  solved  :: b -> a -> Bool
  choices :: b -> a -> [a]

solve :: (Puzzle a b, MonadPlus m) => b -> a -> m a
solve info p
  | solved info p  = return p
  | otherwise = msum $ map (solve info) $ choices info p

explain :: (Puzzle a b, MonadPlus m) => b -> a -> m [a]
explain info p = f [p]
  where
    f (p : ps)
      | solved info p  = return $ p : ps
      | otherwise = msum $ map (f . (: (p : ps))) $ choices info p

data Test = Test Int deriving (Show)

instance Puzzle Test () where
  solved _ (Test 0) = True
  solved _ _ = False
  choices _ (Test n)
    | n <= 1    = []
    | otherwise = [Test $ n - 1, Test $ n - 2]

