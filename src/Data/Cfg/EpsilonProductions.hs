-- | Functionality for detecting and removing epsilon-productions.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.EpsilonProductions (
    isEpsilonFree,
    removeEpsilonProductions
    ) where

import Control.Monad
import Data.Cfg.Cfg
import Data.Cfg.FreeCfg
import Data.Cfg.Nullable
import qualified Data.Set as S

-- | A slight misnomer: returns true if the analysis's grammar is
-- epsilon-free /except/ possibly at the start symbol.
isEpsilonFree :: (Cfg cfg t nt, Ord nt) => cfg t nt -> Bool
isEpsilonFree cfg = S.null ns
    where
    ns = startSymbol cfg `S.delete` nullables cfg

-- | Given a grammar, returns its augmented grammar with
-- epsilon-productions removed.
removeEpsilonProductions :: forall t nt
			 . (Ord nt, Ord t)
			 => FreeCfg t nt
			 -> FreeCfg t nt
removeEpsilonProductions cfg = FreeCfg {
    nonterminals' = nonterminals cfg,
    terminals' = terminals cfg,
    productionRules' = prods',
    startSymbol' = startSymbol cfg
    }
    where
    prods' :: nt -> S.Set (Vs t nt)
    prods' nt' = S.fromList
		     $ filter (\ vs -> isStart || not (null vs))
			 $ concatMap expandRhs
			     $ S.toList
				 $ productionRules cfg nt'
	where
	isStart = nt' == startSymbol cfg

	expandRhs :: Vs t nt -> [Vs t nt]
	expandRhs [] = return []
	expandRhs (v@(T _) : vs) = liftM (v:) $ expandRhs vs
	expandRhs (v@(NT nt) : vs) = if nt `S.member` nullables cfg
				 then vExpandedVs `mplus` expandedVs
                                 else vExpandedVs
            where
            vExpandedVs = liftM (v:) expandedVs
            expandedVs = expandRhs vs
