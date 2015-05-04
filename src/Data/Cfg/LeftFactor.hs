-- | Left factoring of grammars.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftFactor (
    hasLeftFactors
    ) where

import Data.Cfg.Cfg(Cfg, Vs, productionMap)
import qualified Data.Map as M
import qualified Data.Set as S

-- | True if the grammar has multiple productions of the same
-- nonterminal whose right-hand sides start with the same vocabulary
-- element.
hasLeftFactors :: (Cfg cfg t nt, Ord nt, Ord t)
	      => cfg t nt -> Bool
hasLeftFactors cfg = any hasLeftFactorsRhss (M.elems $ productionMap cfg)

hasLeftFactorsRhss :: forall nt t
		  . (Ord nt, Ord t)
		  => S.Set (Vs t nt) -> Bool
hasLeftFactorsRhss rhsSet = S.size rhsSet /= S.size (S.map safeHead rhsSet)
    where
    safeHead :: [a] -> Maybe a
    safeHead as = case as of
		      [] -> Nothing
		      a : _ -> Just a


-- leftFactor :: Cfg cfg t nt => cfg t nt -> FreeCfg t nt
