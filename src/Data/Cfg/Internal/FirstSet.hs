-- | First sets of a context-free grammar.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Internal.FirstSet(firstSetMap, firstsOfVs) where

import Data.Cfg.Augment
import Data.Cfg.Cfg
import Data.Cfg.FixedPoint(fixedPoint)
import Data.Cfg.LookaheadSet hiding(unions)
import qualified Data.Cfg.LookaheadSet as LA
import qualified Data.Map.Strict as M
import Data.Maybe(fromMaybe)
import qualified Data.Set as S

-- | Returns the first set of the nonterminal for the grammar as a
-- map.
firstSetMap :: forall cfg t nt
        . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t)
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

firstsV :: Ord nt
    => M.Map (AugNT nt) (LookaheadSet t) -> V (AugT t) (AugNT nt)
                         -> LookaheadSet t
firstsV _ (T t) = LA.singleton t
firstsV fs (NT nt) = fromMaybe LA.empty (M.lookup nt fs)

    -- TODO I need a consistent story here of what I define and
    -- export.  FollowSet needs this one below, but you can see the
    -- code duplication with firstsV.  Resolve.

-- | Given a firsts function, find the first set of a list of symbols.
firstsOfVs :: Ord t
       => (AugNT nt -> LookaheadSet t) -> AugVs t nt -> LookaheadSet t
firstsOfVs firsts vs = mconcat $ map firstsV' vs
    where
    firstsV' (T t) = LA.singleton t
    firstsV' (NT nt) = firsts nt