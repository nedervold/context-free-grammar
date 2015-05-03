{-# LANGUAGE FlexibleContexts #-} -- for Eq (gr n e)
module Data.Cfg.SCComp (
    SCComp(..),
    ) where

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

