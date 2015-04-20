-- | Depth-first search
module Data.Graph.Inductive.ULGraph.Query.DFS (
    -- * Functions over 'Context's
    CFun,
    -- * Depth-first search giving nodes
    dfs,
    dfs',
    dff,
    dff',
    -- * Depth-first search giving arbitrary types
    dfsWith,
    dfsWith',
    dffWith,
    dffWith',
    -- * Depth-first search with control of successors
    xdfsWith,
    xdfWith,
    xdffWith,
    -- * Applications of depth-first search
    reachable,
    scc,
    topsort
    ) where

import Data.Graph.Inductive.ULGraph
import Data.Tree
import qualified Data.Graph.Inductive.Graph as G
import qualified Data.Graph.Inductive.Query.DFS as G

-- | A function from a 'Context' to a result type
type CFun n e r = Context n e -> r

-- | Converts an FLG 'Data.Graph.Inductive.Query.DFS.CFun' into a
-- 'CFun'.
toFglCFun :: ULGraph gr n e -> CFun n e r -> G.CFun n e r
toFglCFun gr f fglCtxt = f $ fromFglContext gr fglCtxt

-- | Depth-first search from a list of node, resulting in a list of
-- nodes visited.
dfs :: (G.Graph gr, Ord n) => [n] -> ULGraph gr n e -> [n]
dfs ns gr = map (toLabel gr) $ G.dfs (map (toNode gr) ns) $ toGraph gr

-- | Depth-first search from all nodes of a graph, resulting in a list
-- of nodes visited.
dfs' :: G.Graph gr => ULGraph gr n e -> [n]
dfs' gr = map (toLabel gr) $ G.dfs' $ toGraph gr

-- | Depth-first forest from a list of nodes, resulting in the forest
-- of nodes visited.
dff :: (G.Graph gr, Ord n) => [n] -> ULGraph gr n e -> [Tree n]
dff ns gr = fmap (fmap $ toLabel gr) $ G.dff (map (toNode gr) ns) $ toGraph gr

-- | Depth-first forest from all nodes of a graph, resulting in the
-- forest of nodes visited.
dff' :: G.Graph gr => ULGraph gr n e -> [Tree n]
dff' gr = fmap (fmap $ toLabel gr) $ G.dff' $ toGraph gr

-- | Depth-first search from a list of nodes, using the 'CFun' to
-- generate the list of results.
dfsWith :: (G.Graph gr, Ord n) => CFun n e r -> [n] -> ULGraph gr n e -> [r]
dfsWith f ns gr = G.dfsWith (toFglCFun gr f) (map (toNode gr) ns) (toGraph gr)

-- | Depth-first search from all nodes of a graph, using the 'CFun' to
-- generate the list of results.
dfsWith' :: G.Graph gr => CFun n e r -> ULGraph gr n e -> [r]
dfsWith' f gr = G.dfsWith' (toFglCFun gr f) (toGraph gr)

-- | Depth-first forest from a list of nodes, using the 'CFun' to
-- generate the forest of results.
dffWith :: (G.Graph gr, Ord n)
	=> CFun n e r -> [n] -> ULGraph gr n e -> [Tree r]
dffWith f ns gr = G.dffWith (toFglCFun gr f) (map (toNode gr) ns) (toGraph gr)

-- | Depth-first forest from all nodes of a graph, using the 'CFun' to
-- generate the forest of results.
dffWith' :: G.Graph gr => CFun n e r -> ULGraph gr n e -> [Tree r]
dffWith' f gr = G.dffWith' (toFglCFun gr f) (toGraph gr)

-- | Generalized depth-first search from a list of nodes, using a
-- 'CFun' to generate the successor nodes and a 'CFun' to generate the
-- list of results.
xdfsWith :: (G.Graph gr, Ord n)
	 => CFun n e [n] -> CFun n e r -> [n] -> ULGraph gr n e -> [r]
xdfsWith sucF f ns gr = G.xdfsWith (map (toNode gr) . toFglCFun gr sucF)
				   (toFglCFun gr f)
				   (map (toNode gr) ns)
				   (toGraph gr)

-- | Generalized depth-first forest from a list of nodes, using a
-- 'CFun' to generate the successor nodes and a 'CFun' to generate the
-- forest of results.  Returns the forest and the remaining graph
-- after the given nodes are visited.
xdfWith :: (G.Graph gr, Ord n)
	 => CFun n e [n] -> CFun n e r -> [n] -> ULGraph gr n e
	 -> ([Tree r], ULGraph gr n e)
xdfWith sucF f ns gr = (forest, fromGraph gr'')
    where
    (forest, gr'') = G.xdfWith (map (toNode gr) . toFglCFun gr sucF)
			       (toFglCFun gr f)
			       (map (toNode gr) ns)
			       (toGraph gr)

-- | Generalized depth-first forest from a list of nodes, using a
-- 'CFun' to generate the successor nodes and a 'CFun' to generate the
-- forest of results.
xdffWith :: (G.Graph gr, Ord n)
	 => CFun n e [n] -> CFun n e r -> [n] -> ULGraph gr n e
	 -> [Tree r]
xdffWith sucF f ns gr = fst $ xdfWith sucF f ns gr

-- | Topological sorting of the graph's nodes
topsort :: G.Graph gr => ULGraph gr n e -> [n]
topsort = G.topsort' . toGraph

-- | The strongly-connected components of the graph
scc :: G.Graph gr => ULGraph gr n e -> [[n]]
scc gr = fmap (fmap (toLabel gr)) $ G.scc $ toGraph gr

-- | The nodes of the graph reachable from the given node
reachable :: (G.Graph gr, Ord n) => n -> ULGraph gr n e -> [n]
reachable src gr = map (toLabel gr) $ G.reachable (toNode gr src) (toGraph gr)