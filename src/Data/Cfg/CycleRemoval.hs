-- | Removal of cycles from a grammar.
{-# LANGUAGE FlexibleContexts #-} -- for Eq (gr n e)
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.CycleRemoval(
    SCComp(..),
    removeCycles,
    ) where

import Control.Monad.State
import Data.Cfg.Cfg(Cfg, Production)
import Data.Cfg.FreeCfg(FreeCfg, withProductions)
import Data.Graph.Inductive.Graph(Graph)
import Data.Graph.Inductive.ULGraph
import Data.Ord(comparing)
import qualified Data.Set as S

-- | Strongly-connected components divided up by type.
data SCComp gr n e = SCComp (ULGraph gr n e)
	-- ^ a strongly-connected subgraph
    | SelfLoop n (S.Set e)
	-- ^ a component with a single component that links to itself
    | Singleton n
	-- ^ a component with a single component
    deriving (Eq)

instance (Eq e, Eq (gr n e), Ord n) => Ord (SCComp gr n e) where

    -- We assume here that all the SCComp are part of the same
    -- decomposition.  This implies that the nodes don't repeat
    -- between components, and so ordering on the set of nodes is a
    -- consistent ordering.  This may not be what I want in the
    -- future, though.
    --
    -- TODO Investigate this

    compare = comparing f
	where
	f :: SCComp gr n e -> (Int, S.Set n)
	f (SCComp gr) = (0, nodes gr)
	f (SelfLoop n _) = (1, S.singleton n)
	f (Singleton n) = (2, S.singleton n)

-- | Removes cycles from a context-free grammar.  This is a
-- generalization of the algorithm used by Paull to remove
-- left-recursion from a grammar.  Each strongly-connected component
-- of the graph is processed one by one.  @direct@ is called to remove
-- loops from its argument to itself.  @indirect@ is called to remove
-- edges from its first argument to its second.	 It may substitute new
-- edges to later components.
removeCycles :: forall cfg e gr n nt t
	     . (Cfg cfg t nt, Graph gr, Ord nt, Ord t)
	     => (n -> n -> [Production t nt] -> [Production t nt])
		    -- ^ indirect
	     -> (n -> [Production t nt] -> [Production t nt]) -- ^ direct
	     -> [SCComp gr n e] -- ^ components of the graph
	     -> cfg t nt -- ^ the grammar
	     -> FreeCfg t nt
removeCycles removeIndirect removeDirect sccs
    = withProductions removeCyclesProductions

    where
    removeCyclesProductions :: [Production t nt] -> [Production t nt]
    removeCyclesProductions ps
	= foldl (flip removeCyclesSccProductions) ps sccs

    removeCyclesSccProductions :: SCComp gr n e -> [Production t nt]
						-> [Production t nt]
    removeCyclesSccProductions (SCComp gr) ps = flip execState ps $
	forM_ [0 .. n-1] $ \ i -> do
	    let n_i = ns !! i
	    forM_ [0 .. i-1] $ \ j -> do
		let n_j = ns !! j
		modify $ removeIndirect n_i n_j
	    modify $ removeDirect n_i
        where
        n = length ns
        ns = S.toList $ nodes gr
    removeCyclesSccProductions (SelfLoop n _) ps = removeDirect n ps
    removeCyclesSccProductions (Singleton _) ps = ps

