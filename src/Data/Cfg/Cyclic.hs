-- | Functionality for detecting and removing cycles.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Cyclic (
    isCyclic,
    removeCycles,
    removeCycles'
    ) where

import Data.Cfg.Cfg(Cfg, Production(..), ProductionMap, V(..), Vs,
    lookupProductions, productions)
import qualified Data.Cfg.CycleRemoval as CR
import Data.Cfg.CycleRemoval(SCComp(..))
import Data.Cfg.EpsilonProductions(removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg, toFreeCfg)
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Graph.Inductive.ULGraph
import Data.Graph.Inductive.ULGraph.Query.DFS(scc)
import qualified Data.Map as M
import qualified Data.Set as S

-- | Is the grammar cyclic?  That is, is there a NT @a@ such that @a
-- =>+ a@?
isCyclic :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> Bool
isCyclic cfg = case S.toList
			$ cycSccs
			    $ removeEpsilonProductions
				$ toFreeCfg cfg of
		   SCComp _ : _ -> True
		   SelfLoop _ _ : _ -> True
		   _ -> False

-- | Produces an equivalent non-cyclic grammar.
removeCycles :: forall cfg nt t
	     . (Cfg cfg t nt, Ord nt, Ord t)
	     => cfg t nt -> FreeCfg t nt
removeCycles cfg = CR.removeCycles indirect direct (S.toList $ cycSccs cfg) cfg
    where
    indirect :: nt -> nt -> [Production t nt] -> [Production t nt]
    indirect src dst ps = do
	p <- ps
	if isCyclicEdge src dst p
	    then do
		rhs <- lookupProductions dst ps
		return $ Production src rhs
	    else return p

    isCyclicEdge :: nt -> nt -> Production t nt -> Bool
    isCyclicEdge src dst (Production src' [NT dst'])
	= src == src' && dst == dst'
    isCyclicEdge _ _ _ = False

    isCyclicLoop :: nt -> Production t nt -> Bool
    isCyclicLoop nt = isCyclicEdge nt nt

    direct :: nt -> [Production t nt] -> [Production t nt]
    direct nt = filter (not . isCyclicLoop nt)

-- | Produces an equivalent non-cyclic grammar.
removeCycles' :: forall cfg nt t
	     . (Cfg cfg t nt, Ord nt, Ord t)
	     => cfg t nt -> FreeCfg t nt
removeCycles' cfg
    = CR.removeCycles' indirect direct (S.toList $ cycSccs cfg) cfg
    where
    indirect :: nt -> nt -> ProductionMap t nt -> ProductionMap t nt
    indirect src dst pm = M.adjust f src pm
	where
	f :: S.Set (Vs t nt) -> S.Set (Vs t nt)
	f rhss = if [NT dst] `S.member` rhss
		     then [NT dst] `S.delete` rhss `S.union` (pm M.! dst)
		     else rhss

    direct :: nt -> ProductionMap t nt -> ProductionMap t nt
    direct nt = M.adjust removeLoop nt
	where
	removeLoop = S.filter (/= [NT nt])

cycSccs :: forall cfg nt t
       . (Cfg cfg t nt, Ord nt)
       => cfg t nt
       -> S.Set (SCComp Gr nt ())
cycSccs cfg = S.fromList $ map categorizeScc $ scc gr
    where
    gr :: ULGraph Gr nt ()
    gr = mkCycGraph cfg

    categorizeScc :: [nt] -> SCComp Gr nt ()
    categorizeScc [] = error "lrSccs.categorizeScc [] : impossible"
    categorizeScc [nt] = if hasSelfLoop nt
	then SelfLoop nt $ S.singleton ()
	else Singleton nt
    categorizeScc ns = SCComp $ delNodes others gr
	where
	others = S.toList (nodes gr S.\\ S.fromList ns)

    hasSelfLoop :: nt -> Bool
    hasSelfLoop nt = nt `elem` suc gr nt

type E nt = (nt, nt, ())

mkCycGraph :: (Cfg cfg t nt, Ord nt)
           => cfg t nt -> ULGraph Gr nt ()
mkCycGraph = mkULGraph [] . makeEdges

makeEdges :: forall cfg t nt
          . (Cfg cfg t nt, Ord nt)
          => cfg t nt -> [E nt]
makeEdges cfg = [ (nt, nt', ()) | Production nt [NT nt'] <- productions cfg]
