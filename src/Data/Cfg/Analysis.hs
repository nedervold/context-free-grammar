-- | Analysis of a context-free grammar
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Analysis (
    Analysis(..),
    mkAnalysis,
    Prediction,
    Predictions
    ) where

import Data.Cfg.Augment
import qualified Data.Cfg.Cfg as Cfg
import Data.Cfg.FreeCfg
import qualified Data.Cfg.Internal.FirstSet as I
import qualified Data.Cfg.Internal.FollowSet as I
import qualified Data.Cfg.Internal.Nullable as I
import qualified Data.Cfg.Internal.PredictSet as I
import Data.Cfg.Internal.PredictSet(Prediction, Predictions)
import Data.Cfg.LookaheadSet
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Analysis of a context-free grammar
data Analysis t nt = Analysis {
    baseCfg :: FreeCfg t nt,
    -- ^ (a 'FreeCfg' equivalent to) the source grammar
    augmentedCfg :: FreeCfg (AugT t) (AugNT nt),
    -- ^ the augmented grammar
    nullables :: S.Set (AugNT nt),
    -- ^ the nonterminals in the grammar that can produce the
    -- empty string
    firstSet :: AugNT nt -> LookaheadSet t,
    -- ^ the first set of the nonterminal for the grammar
    firstsOfVs :: AugVs t nt -> LookaheadSet t,
    -- ^ the first set of a list of symbols
    followSet :: AugNT nt -> LookaheadSet t,
    -- ^ the follow set of the nonterminal for the grammar
    predictSet :: AugProduction t nt -> LookaheadSet t,
    -- ^ the predict set of the production
    isLL1 :: Bool,
    -- ^ 'True' iff the grammar is LL(1)
    ll1Info :: AugNT nt -> Predictions t nt
    -- ^ the productions for this nonterminal and the lookaheads
    -- that predict them
    }

-- | Analyzes a context-free grammar
mkAnalysis :: forall cfg t nt
       . (Cfg.Cfg cfg t nt, Ord nt, Ord t)
       => cfg t nt -> Analysis t nt
mkAnalysis cfg = Analysis {
    baseCfg = bcfg,
    augmentedCfg = cfg',
    nullables = ns,
    firstSet = fs,
    firstsOfVs = I.firstsOfVs fs,
    followSet = fols,
    predictSet = predict,
    isLL1 = isLL1',
    ll1Info = (ll1InfoMap M.!)
    }
    where
    bcfg = toFreeCfg cfg
    cfg' = augmentCfg bcfg
    ns = I.nullables cfg'
    fsm = I.firstSetMap cfg'
    fs nt = fsm M.! nt
    folm = I.followSetMap cfg' fs
    fols nt = folm M.! nt
    predict = I.predictSet fs fols
    ll1InfoMap = I.ll1InfoMap cfg' predict
    isLL1' = I.isLL1 ll1InfoMap