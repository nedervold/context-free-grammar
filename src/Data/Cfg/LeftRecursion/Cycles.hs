-- | Calculating the strongly-connected components of the
-- left-recursion graph.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftRecursion.Cycles (
    lrSccs
    ) where

import Data.Cfg.Cfg
import Data.Cfg.CycleRemoval(SCComp(..))
import Data.Cfg.Item
import Data.Cfg.Nullable
import Data.Graph.Inductive.PatriciaTree(Gr)
import Data.Graph.Inductive.ULGraph hiding (empty)
import Data.Graph.Inductive.ULGraph.Query.DFS(scc)
import qualified Data.Set as S

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

-- | Strongly-connected components of the left-recursion graph.
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

