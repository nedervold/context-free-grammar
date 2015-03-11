-- | First sets of a context-free grammar.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.FirstSet(firstSet, firstSetMap) where

import Data.Cfg.Augment
import Data.Cfg.Cfg
import Data.Cfg.FixedPoint(fixedPoint)
import Data.Cfg.LookaheadSet hiding(unions)
import qualified Data.Cfg.LookaheadSet as LA
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import Data.Monoid(Monoid(..))
import qualified Data.Set as S

firstSetMap :: forall cfg t nt
	    . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t, Show nt)
	    => cfg (AugT t) (AugNT nt) -> M.Map (AugNT nt) (LookaheadSet t)
firstSetMap cfg = fixedPoint go M.empty
    where
    go :: M.Map (AugNT nt) (LookaheadSet t)
       -> M.Map (AugNT nt) (LookaheadSet t)
    go knownFirsts
	= M.fromList [(nt, firstAlts rhss)
	      | nt <- S.toList $ nonterminals cfg,
		let rhss = S.toList $ productionRules cfg nt,
		not $ null rhss ]
	where
	firstAlts :: [Vs (AugT t) (AugNT nt)] -> LookaheadSet t
	firstAlts = LA.unions . map (mconcat . map (firstsV knownFirsts))

-- | Returns the first set of the nonterminal for the grammar.	To
-- avoid recalculations, hold a copy of @firstSet cfg@.
firstSet :: forall cfg t nt
	 . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t, Show nt)
	 => cfg (AugT t) (AugNT nt) -> AugNT nt -> LookaheadSet t
firstSet cfg nt = firstSetMap cfg M.! nt

firstsV :: Ord nt
	=> M.Map (AugNT nt) (LookaheadSet t) -> V (AugT t) (AugNT nt)
					     -> LookaheadSet t
firstsV _ (T t) = LA.singleton t
firstsV fs (NT nt) = fromMaybe LA.empty (M.lookup nt fs)
