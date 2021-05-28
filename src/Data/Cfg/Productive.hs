-- | Productivity of productions in the grammar.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Productive (
    productives,
    unproductives,
    removeUnproductives
    ) where

import Control.Monad(guard, unless)
import Data.Cfg.Cfg(Cfg(..), Production, V(..), Vs, productions)
import Data.Cfg.FixedPoint(fixedPoint)
import Data.Cfg.FreeCfg(FreeCfg(..))
import qualified Data.Set as S

-- | Returns the productive productions of this grammar.
productives :: forall cfg t nt
        . (Cfg cfg t nt, Ord nt, Ord t)
        => cfg t nt -> S.Set (Production t nt)
productives cfg = S.fromList
              $ filter (isProductiveProduction productiveNTs)
              $ productions cfg
    where
    productiveNTs :: S.Set nt
    productiveNTs = productiveNonterminals cfg

-- | Returns the unproductive productions of this grammar.
unproductives :: forall cfg t nt
          . (Cfg cfg t nt, Ord nt, Ord t)
          => cfg t nt -> S.Set (Production t nt)
unproductives cfg = S.fromList (productions cfg) S.\\ productives cfg

-- | Returns an equivalent grammar not including unproductive
-- productions.
removeUnproductives :: forall cfg t nt
            . (Cfg cfg t nt, Ord nt, Ord t)
            => cfg t nt -> FreeCfg t nt
removeUnproductives cfg = FreeCfg {
    terminals' = terminals cfg,
    startSymbol' = startSymbol cfg,
    nonterminals' = nts,
    productionRules' = rules
    }
    where
    nts :: S.Set nt
    nts = productiveNonterminals cfg

    rules :: nt -> S.Set (Vs t nt)
    rules nt = if nt `S.member` nts
    then S.filter (isProductiveVs nts) $ productionRules cfg nt
    else S.empty

-- | Returns the productive nonterminals of the grammar
productiveNonterminals :: forall cfg t nt
               . (Cfg cfg t nt, Ord nt, Ord t)
               => cfg t nt -> S.Set nt
productiveNonterminals cfg = fixedPoint f S.empty
    where
    f :: S.Set nt -> S.Set nt
    f productiveNTs = S.fromList $ do
    nt <- S.toList $ nonterminals cfg
    unless (nt `S.member` productiveNTs) $ do
        let rhss = productionRules cfg nt
        guard (any (isProductiveVs productiveNTs) $ S.toList rhss)
    return nt

isProductiveProduction :: forall t nt
               . (Ord nt)
               => S.Set nt -> Production t nt -> Bool
isProductiveProduction productiveNTs (hd, rhs)
    = hd `S.member` productiveNTs
      && isProductiveVs productiveNTs rhs

-- | Given a set of known productive nonterminals, is the vocabulary
-- symbol productive?
isProductiveVs :: forall t nt
           . (Ord nt)
           => S.Set nt -> Vs t nt -> Bool
isProductiveVs productiveNTs = all isProductiveV
    where
    -- | Given a set of known productive nonterminals, is the vocabulary
    -- symbol productive?
    isProductiveV :: V t nt -> Bool
    isProductiveV v = case v of
        NT nt -> nt `S.member` productiveNTs
        _ -> True


