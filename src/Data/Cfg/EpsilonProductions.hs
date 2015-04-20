-- | Functionality for detecting and removing epsilon-productions.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.EpsilonProductions (
    removeEpsilonProductions
    ) where

import Control.Monad
import Data.Cfg.Analysis
import Data.Cfg.Augment
import Data.Cfg.Cfg
import Data.Cfg.FreeCfg
import qualified Data.Set as S

-- | Given a grammar, returns its augmented grammar with
-- epsilon-productions removed.
removeEpsilonProductions :: forall t nt
			 . (Ord nt, Ord t)
			 => FreeCfg t nt
			 -> FreeCfg (AugT t) (AugNT nt)
removeEpsilonProductions cfg = FreeCfg {
    nonterminals' = nonterminals augCfg,
    terminals' = terminals augCfg,
    productionRules' = prods',
    startSymbol' = startSymbol augCfg
    }
    where
    augCfg :: FreeCfg (AugT t) (AugNT nt)
    augCfg = augmentedCfg an

    an :: Analysis t nt
    an = mkAnalysis cfg

    prods' :: AugNT nt -> S.Set (AugVs t nt)
    prods' nt' = S.fromList
		     $ filter (\ vs -> isStart || not (null vs))
			 $ concatMap expandRhs
			     $ S.toList
				 $ productionRules augCfg nt'
	where
	isStart = nt' == StartSymbol

	expandRhs :: AugVs t nt -> [AugVs t nt]
	expandRhs [] = return []
	expandRhs (v@(T _) : vs) = liftM (v:) $ expandRhs vs
	expandRhs (v@(NT nt) : vs) = if nt `S.member` nullables an
                                 then vExpandedVs `mplus` expandedVs
                                 else vExpandedVs
            where
            vExpandedVs = liftM (v:) expandedVs
            expandedVs = expandRhs vs
