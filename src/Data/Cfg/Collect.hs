-- | Data-shuffling
module Data.Cfg.Collect
  ( collectOnFirst
  , collectOnSecond
  , collectOnFirst'
  , collectOnSecond'
  ) where

import Data.List (nub)
import qualified Data.Set as S

-- | Collects a list of pairs on the first element.
collectOnFirst :: Eq a => [(a, b)] -> [(a, [b])]
collectOnFirst pairs = [(a, bsFor a) | a <- as]
  where
    as = nub $ map fst pairs
    bsFor a = [b | (a', b) <- pairs, a == a']

-- | Collects a list of pairs on the second element.
collectOnSecond :: Eq b => [(a, b)] -> [([a], b)]
collectOnSecond pairs = [(asFor b, b) | b <- bs]
  where
    bs = nub $ map snd pairs
    asFor b = [a | (a, b') <- pairs, b == b']

-- | Collects a list of pairs on the first element.
collectOnFirst' :: (Eq a, Ord b) => [(a, b)] -> [(a, S.Set b)]
collectOnFirst' pairs = [(a, bsFor a) | a <- as]
  where
    as = nub $ map fst pairs
    bsFor a = S.fromList [b | (a', b) <- pairs, a == a']

-- | Collects a list of pairs on the second element.
collectOnSecond' :: (Ord a, Eq b) => [(a, b)] -> [(S.Set a, b)]
collectOnSecond' pairs = [(asFor b, b) | b <- bs]
  where
    bs = nub $ map snd pairs
    asFor b = S.fromList [a | (a, b') <- pairs, b == b']
