{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | The free 'Cfg'
module Data.Cfg.FreeCfg (
    -- * free context-free grammars
    FreeCfg(..),
    bimapCfg,
    toFreeCfg,
    -- * productions
    fromProductions,
    withProductions,
    -- * production maps
    fromProductionMap,
    withProductionMap,
    withProductionMapM
    ) where

import Control.Monad(liftM)
import Data.Cfg.Cfg(Cfg(..), Production(..), ProductionMap, V(..), Vs,
    bimapProductions, lookupProductions, productionMap, productions)
import qualified Data.Map as M
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
	f (Production hd rhs) = NT hd : rhs

    nts :: S.Set nt
    nts = S.fromList [ nt | NT nt <- vs ]

    ts :: S.Set t
    ts = S.fromList [ t | T t <- vs ]

    rules :: nt -> S.Set (Vs t nt)
    rules = S.fromList . flip lookupProductions prods

-- | Maps over the terminal and nonterminal symbols in a context-free
-- grammar, returning a 'FreeCfg' over the new symbols.
bimapCfg :: (Cfg cfg t nt, Ord nt', Ord t')
	 => (t -> t') -> (nt -> nt')
	 -> cfg t nt -> FreeCfg t' nt'
bimapCfg f g cfg = fromProductions (g $ startSymbol cfg)
		       $ bimapProductions f g $ productions cfg

-- | Lifts a function from 'Production's to 'Cfg's
withProductions :: (Cfg cfg t nt, Ord nt, Ord t)
		=> ([Production t nt] -> [Production t nt])
		-> cfg t nt -> FreeCfg t nt
withProductions f cfg = fromProductions (startSymbol cfg) $ f $ productions cfg

------------------------------------------------------------

-- | Creates a 'FreeCfg' from a start symbol and a 'ProductionMap'.
-- Assumes the vocabulary items are exactly those present in the
-- 'ProductionMap'.
fromProductionMap :: (Ord nt, Ord t)
		  => nt -> ProductionMap t nt -> FreeCfg t nt
fromProductionMap ss pm = fromProductions ss $ do
    (nt, rhss) <- M.toList pm
    rhs <- S.toList rhss
    return $ Production nt rhs

-- | Lifts a function from 'ProductionMap's to 'Cfg's
withProductionMap :: (Cfg cfg t nt, Ord nt, Ord t)
		  => (ProductionMap t nt -> ProductionMap t nt)
		  -> cfg t nt -> FreeCfg t nt
withProductionMap f cfg
    = fromProductionMap (startSymbol cfg) $ f $ productionMap cfg

-- | Lifts a function from 'ProductionMap's to 'Cfg's
withProductionMapM :: (Cfg cfg t nt, Monad m, Ord nt, Ord t)
		   => (ProductionMap t nt -> m (ProductionMap t nt))
                   -> cfg t nt -> m (FreeCfg t nt)
withProductionMapM f cfg
    = liftM (fromProductionMap (startSymbol cfg)) $ f $ productionMap cfg
