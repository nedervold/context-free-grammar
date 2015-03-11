-- | Augmented grammars.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Augment (
    -- * Augmenting grammars
    augmentCfg,
    -- * Augmenting symbols
    AugNT(..),
    AugT(..),
    ) where

import Data.Cfg.Cfg(Cfg(..), V(..), Vs)
import Data.Cfg.FreeCfg(FreeCfg(..))
import qualified Data.Set as S

-- | Nonterminal symbols augmented with a special 'StartSymbol'
data AugNT nt = StartSymbol | AugNT nt
    deriving (Eq, Ord, Show)

-- | Terminal symbols augmented with a special end-of-file symbol
data AugT t = EOF | AugT t
    deriving (Eq, Ord, Show)

-- | Returns the /augmented/ grammar: a grammar for the same language
-- but using explicit start and end-of-file symbols.
augmentCfg :: forall cfg t nt . (Cfg cfg t nt, Ord nt, Ord t)
	   => cfg t nt -> FreeCfg (AugT t) (AugNT nt)
augmentCfg cfg = FreeCfg {
    nonterminals' = S.insert StartSymbol $ S.map AugNT $ nonterminals cfg,
    terminals' = S.insert EOF $ S.map AugT $ terminals cfg,
    productionRules' = pr,
    startSymbol' = StartSymbol
    }

    where
    pr :: AugNT nt -> S.Set (Vs (AugT t) (AugNT nt))
    pr StartSymbol = S.singleton [NT $ AugNT $ startSymbol cfg, T EOF]
    pr (AugNT nt) = S.map augmentVs oldRhss
	where
	oldRhss :: S.Set (Vs t nt)
	oldRhss = productionRules cfg nt

	augmentVs :: Vs t nt -> Vs (AugT t) (AugNT nt)
	augmentVs = map augmentV

	augmentV :: V t nt -> V (AugT t) (AugNT nt)
	augmentV (NT nt') = NT $ AugNT nt'
        augmentV (T t') = T $ AugT t'
