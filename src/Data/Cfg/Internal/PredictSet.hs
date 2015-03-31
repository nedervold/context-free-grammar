{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | Predict sets of a context-free grammar.
module Data.Cfg.Internal.PredictSet (
    Prediction,
    Predictions,
    predictSet,
    ll1InfoMap,
    isLL1
    ) where

import Data.Cfg.Augment
import Data.Cfg.Cfg(Cfg(..))
import Data.Cfg.Collect
import Data.Cfg.Internal.FirstSet(firstsOfVs)
import Data.Cfg.LookaheadSet
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Returns the predict set of a production.
predictSet :: (Ord t)
	   => (AugNT nt -> LookaheadSet t)    -- ^ 'firstSet' for the grammar
	   -> (AugNT nt -> LookaheadSet t)    -- ^ 'followSet' for the grammar
	   -> AugProduction t nt	      -- ^ the production
	   -> LookaheadSet t
predictSet firstSet' followSet' (hd, vs)
    = firstsOfVs firstSet' vs <> followSet' hd

-- | A lookahead set with the productions it predicts
type Prediction t nt = (LookaheadSet t, S.Set (AugProduction t nt))

-- | A set of 'Prediction's.  The 'LookaheadSet's of the 'Prediction's
-- will be pairwise disjoint.
type Predictions t nt = S.Set (Prediction t nt)

-- | Returns the production 'Predictions' for the grammar as a map.
ll1InfoMap :: forall cfg t nt
	   . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t)
	   => cfg (AugT t) (AugNT nt)
	   -> (AugProduction t nt -> LookaheadSet t)
	   -> M.Map (AugNT nt) (Predictions t nt)
ll1InfoMap cfg predictSet' = mkMap mkPredictions $ S.toList $ nonterminals cfg
    where
    mkPredictions :: AugNT nt -> Predictions t nt
	-- Mostly reshuffling data
    mkPredictions nt
	= S.fromList $ f $ collectOnSecond $ collectOnFirst' lookaheadProds
	where
	-- Possible lookahead symbols for productions of this nonterminal
	lookaheadProds :: [(AugT t, AugProduction t nt)]
	lookaheadProds	= do
	    rhs <- S.toList $ productionRules cfg nt
	    let prod = (nt, rhs)
	    t <- S.toList $ toSet $ predictSet' prod
	    return (t, prod)

	f :: [([AugT t], S.Set (AugProduction t nt))]
	  -> [(LookaheadSet t, S.Set (AugProduction t nt))]
	f pairs = [(fromList la, ps) | (la, ps) <- pairs]

    mkMap :: Ord k => (k -> v) -> [k] -> M.Map k v
    mkMap f ks = M.fromList [(k, f k) | k <- ks]

-- | Returns true iff the predictions are unambiguous, true iff the
-- grammar is LL(1).
isLL1 :: M.Map (AugNT nt) (Predictions t nt) -> Bool
isLL1 m = all (\ ps -> S.size ps == 1) $ M.elems m
