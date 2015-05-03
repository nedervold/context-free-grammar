-- | Functionality for detecting and removing cycles.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Cyclic (
    isCyclic
    ) where

import Data.Cfg.Cfg(Cfg, Production(..), V(..), productions)
import Data.Cfg.FreeCfg(toFreeCfg)
import Data.Cfg.EpsilonProductions(removeEpsilonProductions)
import Data.Cfg.CycleRemoval(SCComp(..))
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Graph.Inductive.ULGraph
import Data.Graph.Inductive.ULGraph.Query.DFS(scc)
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
