-- | Removal of cycles from a grammar.
{-# LANGUAGE FlexibleContexts #-} -- for Eq (gr n e)
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.CycleRemoval(
    SCComp(..),
    removeCycles,
    removeCycles',
    removeCyclesM',
    ) where

import Control.Monad.State
import Data.Cfg.Cfg(Cfg, Production, ProductionMap)
import Data.Cfg.FreeCfg(FreeCfg, withProductionMap, withProductionMapM,
    withProductions)
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

-- | Removes cycles from a context-free grammar.  This is a
-- generalization of the algorithm used by Paull to remove
-- left-recursion from a grammar.  Each strongly-connected component
-- of the graph is processed one by one.  @direct@ is called to remove
-- loops from its argument to itself.  @indirect@ is called to remove
-- edges from its first argument to its second.	 It may substitute new
-- edges to later components.
removeCycles' :: forall cfg e gr n nt t
	      . (Cfg cfg t nt, Graph gr, Ord nt, Ord t)
	      => (n -> n -> ProductionMap t nt -> ProductionMap t nt)
		     -- ^ indirect
	      -> (n -> ProductionMap t nt -> ProductionMap t nt) -- ^ direct
	      -> [SCComp gr n e] -- ^ components of the graph
	      -> cfg t nt -- ^ the grammar
	      -> FreeCfg t nt
removeCycles' removeIndirect removeDirect sccs
    = withProductionMap removeCyclesProductionMap

    where
    removeCyclesProductionMap :: ProductionMap t nt -> ProductionMap t nt
    removeCyclesProductionMap pm
	= foldl (flip removeCyclesSccProductionMap) pm sccs

    removeCyclesSccProductionMap :: SCComp gr n e -> ProductionMap t nt
						  -> ProductionMap t nt
    removeCyclesSccProductionMap (SCComp gr) pm = flip execState pm $
	forM_ [0 .. n-1] $ \ i -> do
	    let n_i = ns !! i
	    forM_ [0 .. i-1] $ \ j -> do
		let n_j = ns !! j
		modify $ removeIndirect n_i n_j
	    modify $ removeDirect n_i
	where
	n = length ns
	ns = S.toList $ nodes gr
    removeCyclesSccProductionMap (SelfLoop n _) pm = removeDirect n pm
    removeCyclesSccProductionMap (Singleton _) pm = pm

-- | Removes cycles from a context-free grammar monadically.  This is
-- a generalization of the algorithm used by Paull to remove
-- left-recursion from a grammar.  Each strongly-connected component
-- of the graph is processed one by one.  @directM@ is called to
-- remove loops from its argument to itself.  @indirectM@ is called to
-- remove edges from its first argument to its second.  It may
-- substitute new edges to later components.
removeCyclesM' :: forall cfg e gr m n nt t
	       . (Cfg cfg t nt, Graph gr, Monad m, Ord nt, Ord t)
	       => (n -> n -> ProductionMap t nt -> m (ProductionMap t nt))
		      -- ^ indirectM
	       -> (n -> ProductionMap t nt -> m (ProductionMap t nt))
		      -- ^ directM
	       -> [SCComp gr n e] -- ^ components of the graph
	       -> cfg t nt -- ^ the grammar
	       -> m (FreeCfg t nt)
removeCyclesM' indirectM directM sccs
    = withProductionMapM removeCyclesProductionMapM

    where
    removeCyclesProductionMapM :: ProductionMap t nt -> m (ProductionMap t nt)
    removeCyclesProductionMapM pm
	= foldM (flip removeCyclesSccProductionMapM) pm sccs

    removeCyclesSccProductionMapM :: SCComp gr n e -> ProductionMap t nt
						   -> m (ProductionMap t nt)
    removeCyclesSccProductionMapM (SCComp gr) pm = flip execStateT pm $
	forM_ [0 .. n-1] $ \ i -> do
	    let n_i = ns !! i
            forM_ [0 .. i-1] $ \ j -> do
                let n_j = ns !! j
                modify' $ indirectM n_i n_j
            modify' $ directM n_i
        where
        n = length ns
        ns = S.toList $ nodes gr

    removeCyclesSccProductionMapM (SelfLoop n _) pm = directM n pm
    removeCyclesSccProductionMapM (Singleton _) pm = return pm

    modify' :: (s -> m s) -> StateT s m ()
    modify' f = do
        s <- get
        s' <- lift $ f s
        put s'
