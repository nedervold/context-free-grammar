{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The free 'Cfg'
module Data.Cfg.FreeCfg (
    FreeCfg(..),
    fromProductions,
    toFreeCfg
    ) where

import Data.Cfg.Cfg(Cfg(..), Production, V(..), Vs)
import qualified Data.Set as S

-- | Represents a context-free grammar with its nonterminal and
-- terminal types.  The canonical instance of 'Cfg': a record that
-- collects up implementations of each class method.
data FreeCfg t nt = FreeCfg {
    nonterminals' :: S.Set nt,
	-- ^ the nonterminals of the grammar
    terminals' :: S.Set t,
	-- ^ the terminals of the grammar
    productionRules' :: nt -> S.Set (Vs t nt),
	-- ^ the productions of the grammar
    startSymbol' :: nt
	-- ^ the start symbol of the grammar; must be an element of
	-- 'nonterminals' 'cfg'
    }

instance Cfg FreeCfg t nt where
    nonterminals = nonterminals'
    terminals = terminals'
    productionRules = productionRules'
    startSymbol = startSymbol'

-- | Converts any 'Cfg' into a 'FreeCfg'.
toFreeCfg :: Cfg cfg t nt => cfg t nt -> FreeCfg t nt
toFreeCfg cfg = FreeCfg {
    nonterminals' = nonterminals cfg,
    terminals' = terminals cfg,
    productionRules' = productionRules cfg,
    startSymbol' = startSymbol cfg
    }

-- | Creates a 'FreeCfg' from a start symbol and a list of
-- 'Production's.  Assumes the vocabulary items are exactly those
-- present in the productions.
fromProductions :: forall nt t
		. (Ord nt, Ord t)
		=> nt -> [Production t nt] -> FreeCfg t nt
fromProductions start prods = FreeCfg {
    nonterminals' = nts,
    terminals' = ts,
    productionRules' = rules,
    startSymbol' = start
    }
    where
    vs :: [V t nt]
    vs = NT start : concatMap f prods
	where
	f (hd, rhs) = NT hd : rhs

    nts = S.fromList [ nt | NT nt <- vs ]
    ts = S.fromList [ t | T t <- vs ]
    rules nt = S.fromList [ rhs | (nt', rhs) <- prods,
                                  nt == nt' ]
