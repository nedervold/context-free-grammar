-- | Predict sets of a context-free grammar.
module Data.Cfg.PredictSet (
    predictSet
    ) where

import Data.Cfg.Augment
import Data.Cfg.FirstSet(firstsOfVs)
import Data.Cfg.LookaheadSet
import Data.Monoid(Monoid(..))

-- | Returns the predict set of a production.
predictSet :: (Ord t)
	   => (AugNT nt -> LookaheadSet t)    -- ^ 'firstSet' for the grammar
	   -> (AugNT nt -> LookaheadSet t)    -- ^ 'followSet' for the grammar
	   -> AugProduction t nt	      -- ^ the production
	   -> LookaheadSet t
predictSet firstSet' followSet' (hd, vs)
    = firstsOfVs firstSet' vs `mappend` followSet' hd

