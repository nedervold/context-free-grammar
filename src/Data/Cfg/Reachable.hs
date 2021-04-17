-- | Reachability of nonterminals in the grammar.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Reachable (
    reachables,
    unreachables,
    removeUnreachables
    ) where

import Data.Cfg.Cfg(Cfg(..), V(..), Vs)
import Data.Cfg.FreeCfg(FreeCfg(..))
import qualified Data.Set as S

-- | Returns the nonterminals of this grammar reachable from the start
-- symbol.
reachables :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
           => cfg t nt -> S.Set nt
reachables cfg = go [startSymbol cfg] S.empty
    where
    go :: [nt] -> S.Set nt -> S.Set nt
    go [] seen = seen
    go (nt : nts) seen
        = if nt `S.member` seen
              then go nts seen
              else do
                  let seen' = S.insert nt seen
                  let vs = concat $ S.toList $ productionRules cfg nt
                  go (nts ++ [nt' | NT nt' <- vs]) seen'

-- | Returns the nonterminals of this grammar unreachable from the
-- start symbol.
unreachables :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
             => cfg t nt -> S.Set nt
unreachables cfg = nonterminals cfg S.\\ reachables cfg


-- | Returns an equivalent grammar not including unreachable
-- nonterminals.
removeUnreachables :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
                   => cfg t nt -> FreeCfg t nt
removeUnreachables cfg = FreeCfg {
    nonterminals' = res,
    terminals' = terminals cfg,
    productionRules' = pr,
    startSymbol' = startSymbol cfg
    }
    where
    res :: S.Set nt
    res = reachables cfg

    pr :: nt -> S.Set (Vs t nt)
    pr nt = if nt `S.member` res
                then productionRules cfg nt
                else S.empty

