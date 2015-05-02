{-# LANGUAGE FlexibleContexts #-} -- for Eq (gr n e)
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftRecursion.Cycles (
    SCComp(..),
    lrSccs
    ) where

import Data.Cfg.Cfg
import Data.Cfg.Item
import Data.Cfg.Nullable
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.ULGraph hiding (empty)
import Data.Graph.Inductive.ULGraph.Query.DFS
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

type E t nt = (nt, nt, Item t nt)

mkLRGraph :: (Cfg cfg t nt, Ord nt)
	  => cfg t nt -> ULGraph Gr nt (Item t nt)
mkLRGraph = mkULGraph [] . makeEdges

makeEdges :: forall cfg t nt
	  . (Cfg cfg t nt, Ord nt)
	  => cfg t nt -> [E t nt]
makeEdges cfg = map itemEdge allItems
    where
    allItems :: [Item t nt]
    allItems = do
	Production nt rhs <- productions cfg
	items isNullable (Production nt rhs)

    itemEdge :: Item t nt -> E t nt
    itemEdge item = (hdNode, ntNode, item)
	where
	hdNode = productionHead $ production item
	ntNode = nt
	    where
	    Just (NT nt) = nextV item

    isNullable :: nt -> Bool
    isNullable nt = nt `S.member` nullables cfg

items :: forall t nt . (nt -> Bool) -> Production t nt -> [Item t nt]
items isNullable prod = go $ mkInitialItem prod
    where
    go :: Item t nt -> [Item t nt]
    go item = case nextV item of
	Just (NT nt) -> if isNullable nt
	    then item : maybe [] go (nextItem item)
	    else [item]
	_ -> []

lrSccs :: forall cfg nt t
       . (Cfg cfg t nt, Ord nt, Ord t)
       => cfg t nt
       -> S.Set (SCComp Gr nt (Item t nt))
lrSccs cfg = S.fromList $ map categorizeScc scc'
    where
    categorizeScc :: [nt] -> SCComp Gr nt (Item t nt)
    categorizeScc [] = error "lrSccs.categorizeScc [] : impossible"
    categorizeScc [nt] = if hasSelfLoop nt
	then SelfLoop nt $ S.fromList [ e | (_, dst, e) <- out gr nt,
					    nt == dst ]
	else Singleton nt
    categorizeScc ns = SCComp $ delNodes others gr
	where
	others = S.toList (nodes gr S.\\ S.fromList ns)

    scc' :: [[nt]]
    scc' = scc gr

    gr :: ULGraph Gr nt (Item t nt)
    gr = mkLRGraph cfg

    hasSelfLoop :: nt -> Bool
    hasSelfLoop n = n `elem` suc gr n

