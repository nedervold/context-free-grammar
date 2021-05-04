-- | Graphs with unique labels
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Graph.Inductive.ULGraph(
    -- * ULGraph types
    ULGraph,
    Edge,
    Adj,
    Context,
    MContext,
    Decomp,
    GDecomp,
    -- * Creation
    mkULGraph,
    mkULGraph',
    empty,
    -- * Deletion
    delNode,
    delNodes,
    -- * Query
    isEmpty,
    noNodes,
    equal,
    nodes,
    edges,
    member,
    notMember,
    -- * Decomposition
    match,
    matchAny,
    context,
    -- ** Neighbors
    neighbors,
    suc,
    pre,
    -- ** Edges
    inn,
    out,
    -- ** Degree
    deg,
    indeg,
    outdeg,
    -- * Interface with FGL 'Graph''s
    fromGraph,
    safeFromGraph,
    toGraph,
    toLabelMap,
    toNodeMap,
    toLabel,
    toNode,
    fromFglContext
    ) where

import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Map as M
import Data.Maybe(fromMaybe)
import qualified Data.Set as S
import qualified Data.Traversable as T
import Data.Tuple(swap)

-- | A labeled edge
type Edge n e = (n, n, e)

-- | Labeled links to or from a node.
type Adj n e = [(e, n)]

-- | Links to the node, the node itself, and links from the node
type Context n e = (Adj n e, n, Adj n e)

-- | Converts an FGL 'Data.Graph.Inductive.Graph.Context' to a ULGraph
-- 'Context'.
fromFglContext :: ULGraph gr n e -> G.Context n e -> Context n e
fromFglContext gr (froms, _, n, tos)
    = (fromFglAdj gr froms, n, fromFglAdj gr tos)

-- | Converts an FGL 'Data.Graph.Inductive.Graph.Adj' to a ULGraph
-- 'Adj'.
fromFglAdj :: ULGraph gr n e -> G.Adj e -> Adj n e
fromFglAdj gr adj = [(e, toLabel gr n) | (e, n) <- adj]

-- | A possible 'Context'
type MContext n e = Maybe (Context n e)

-- | 'ULGraph' decomposition: the context removed from a 'ULGraph' and
-- the remaining 'ULGraph'
type Decomp gr n e = (MContext n e, ULGraph gr n e)

-- | The same as 'Decomp', only more sure of itself
type GDecomp gr n e = (Context n e, ULGraph gr n e)

-- | A uniquely labeled graph
data ULGraph gr n e = ULGraph {
    toGraph :: gr n e,
        -- ^ the underlying FGL 'Data.Graph.Inductive.Graph.Graph'
    toNodeMap :: M.Map n G.Node,
        -- ^ the map from the nodes to the FGL
        -- 'Data.Graph.Inductive.Graph.Node's
    toLabelMap :: M.Map G.Node n
        -- ^ the map from the FGL 'Data.Graph.Inductive.Graph.Node's
        -- to the nodes
    }
    deriving (Eq, Ord)

{- TODO: any using DynGraph, equal -}

-- | The node for the label.
toNode :: Ord n => ULGraph gr n e -> n -> G.Node
toNode ulg = (toNodeMap ulg M.!)

-- | The label for the node.
toLabel :: ULGraph gr n e -> G.Node -> n
toLabel ulg = (toLabelMap ulg M.!)

-- | Creates a 'ULGraph' from an FGL
-- 'Data.Graph.Inductive.Graph.Graph'. Throws an error if the nodes
-- are not uniquely labeled.
fromGraph :: (G.Graph gr, Ord n) => gr n e -> ULGraph gr n e
fromGraph gr = fromMaybe err $ safeFromGraph gr
    where
    err = error "fromGraph: graph is not uniquely labeled"

-- | Creates a 'ULGraph' from an FGL
-- 'Data.Graph.Inductive.Graph.Graph' if the nodes are uniquely
-- labeled.
safeFromGraph :: forall gr n e
              . (G.Graph gr, Ord n)
              => gr n e -> Maybe (ULGraph gr n e)
safeFromGraph gr = do
    toNodeMap' <- pairsToMap toNodePairs
    return ULGraph {
        toGraph = gr,
        toNodeMap = toNodeMap',
        toLabelMap = M.fromList toLabelPairs
        }
    where
    toLabelPairs :: [(G.Node, n)]
    toLabelPairs = G.labNodes gr

    toNodePairs :: [(n, G.Node)]
    toNodePairs = map swap toLabelPairs

    pairsToMap :: forall a b . Ord a => [(a, b)] -> Maybe (M.Map a b)
    pairsToMap abs' = T.mapM f m'
        where
        m' :: M.Map a [b]
        m' = M.fromListWith (++) [(a, [b]) | (a, b) <- abs']

        f :: [b] -> Maybe b
        f bs = case bs of
            [b] -> Just b
            _ -> Nothing

-- | An empty 'ULGraph'.
empty :: G.Graph gr => ULGraph gr n e
empty = ULGraph { toGraph = G.empty, toNodeMap = M.empty, toLabelMap = M.empty }

-- | 'True' if the given 'ULGraph' is empty.
isEmpty :: G.Graph gr => ULGraph gr n e -> Bool
isEmpty = G.isEmpty . toGraph

-- | The context for the given node.  The node must be in the graph.
context :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> Context n e
context gr n = case match n gr of
    (Nothing, _) -> error "context: node not in graph"
    (Just c, _) -> c

-- | Decomposes a 'ULGraph' into the 'MContext' found for the given
-- node and the remaining 'ULGraph'.
match :: (G.Graph gr, Ord n) => n -> ULGraph gr n e -> Decomp gr n e
match n gr = (fmap (fromFglContext gr) mc, fromGraph gr')
    where
    (mc, gr') = G.match (toNode gr n) (toGraph gr)

-- | Decomposes a 'ULGraph' into the 'Context' for an arbitrarily
-- chosen node and the remaining 'ULGraph'.
matchAny :: (G.Graph gr, Ord n) => ULGraph gr n e -> GDecomp gr n e
matchAny gr = (fromFglContext gr ctxt, fromGraph gr')
    where
    (ctxt, gr') = G.matchAny $ toGraph gr

-- | Creates a 'ULGraph' from the list of nodes and labeled edges.
-- Nodes need not appear in the list of nodes; it's enough to appear
-- as one end of an edge.
mkULGraph :: forall gr n e
          . (G.Graph gr, Ord n)
          => [n] -> [(n, n, e)] -> ULGraph gr n e
mkULGraph ns es = ULGraph {
    toGraph = G.mkGraph lns les,
    toNodeMap = toNodeMap',
    toLabelMap = M.fromList lns
    }
    where
    ns' :: S.Set n
    ns' = S.fromList $ ns ++ concatMap f es
        where
        f (src, dst, _) = [src, dst]

    lns :: [(G.Node, n)]
    lns = zip [1..] (S.toList ns')

    toNodeMap' :: M.Map n G.Node
    toNodeMap' = M.fromList $ map swap lns

    les :: [G.LEdge e]
    les = [(toNodeMap' M.! src, toNodeMap' M.! dst, e) | (src, dst, e) <- es]

-- | Creates a 'ULGraph' from the list of nodes and unlabeled edges.
-- Nodes need not appear in the list of nodes; it's enough to appear
-- as one end of an edge.
mkULGraph' :: forall gr n
           . (G.Graph gr, Ord n)
           => [n] -> [(n, n)] -> ULGraph gr n ()
mkULGraph' ns es = mkULGraph ns es'
    where
    es' = [(src, dst, ()) | (src, dst) <- es]

-- | The number of nodes in a 'ULGraph'.
noNodes :: G.Graph gr => ULGraph gr n e -> Int
noNodes = M.size . toLabelMap

-- | The set of nodes of the 'ULGraph'
nodes :: ULGraph gr n e -> S.Set n
nodes = M.keysSet . toNodeMap

-- | Are the two 'ULGraph's equal?
equal :: (G.Graph gr, Eq n, Eq e) => ULGraph gr n e -> ULGraph gr n e -> Bool
equal gr gr' = nodes gr == nodes gr' && edges gr == edges gr'

-- | A list of all 'Edge's in the 'ULGraph'.
edges :: G.Graph gr => ULGraph gr n e -> [Edge n e]
edges gr = [(toLabel gr src, toLabel gr dst, e)
               | (src, dst, e) <- G.labEdges $ toGraph gr]

-- | 'True' if the node is in the 'ULGraph'
member :: Ord n => G.Graph gr => ULGraph gr n e -> n -> Bool
member gr n = n `M.member` toNodeMap gr

-- | 'True' if the node is not in the 'ULGraph'
notMember :: Ord n => G.Graph gr => ULGraph gr n e -> n -> Bool
notMember gr n = n `M.notMember` toNodeMap gr

-- | Removes a node from the 'ULGraph'.
delNode :: (G.Graph gr, Ord n)
        => n -> ULGraph gr n e -> ULGraph gr n e
delNode n gr = ULGraph {
    toGraph = G.delNode n' $ toGraph gr,
    toNodeMap = n `M.delete` toNodeMap gr,
    toLabelMap = n' `M.delete` toLabelMap gr
    }
    where
    n' :: G.Node
    n' = toNode gr n

-- | Removes multiple nodes from the 'ULGraph'.
delNodes :: (G.Graph gr, Ord n)
         => [n] -> ULGraph gr n e -> ULGraph gr n e
    -- The quickest to write, but perhaps not the most efficient
delNodes ns gr = foldr delNode gr ns

-- | All nodes that have a link from the given node.
neighbors :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> [n]
neighbors gr n = map (toLabel gr) $ G.neighbors (toGraph gr) (toNode gr n)

-- | All nodes that have a link from the given node.
suc :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> [n]
suc gr n = map (toLabel gr) $ G.suc (toGraph gr) (toNode gr n)

-- | All nodes that have a link to the given node.
pre :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> [n]
pre gr n = map (toLabel gr) $ G.pre (toGraph gr) (toNode gr n)

-- | All outward-bound 'Edge's for the given node.
out :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> [Edge n e]
out gr n = [(toLabel gr src, toLabel gr dst, e)
               | (src, dst, e) <- G.out (toGraph gr) (toNode gr n)]

-- | All inward-bound 'Edge's for the given node.
inn :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> [Edge n e]
inn gr n = [(toLabel gr src, toLabel gr dst, e)
               | (src, dst, e) <- G.inn (toGraph gr) (toNode gr n)]

-- | The outward-bound degree of the node.
outdeg :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> Int
outdeg gr n = G.outdeg (toGraph gr) (toNode gr n)

-- | The inward-bound degree of the node.
indeg :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> Int
indeg gr n = G.indeg (toGraph gr) (toNode gr n)

-- | The degree of the node.
deg :: (G.Graph gr, Ord n) => ULGraph gr n e -> n -> Int
deg gr n = G.deg (toGraph gr) (toNode gr n)

