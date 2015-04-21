-- | Functionality for detecting and removing left-recursion.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftRecursion(
    LR(..),
    SCComp(..),
    isLeftRecursive,
    removeLeftRecursion,
    reportLeftRec) where

import Data.Cfg.Cfg
import Data.Cfg.CPretty
import Data.Cfg.FreeCfg
import Data.Cfg.Item
import Data.Cfg.Nullable
import Data.Either(partitionEithers)
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.ULGraph hiding (empty)
import Data.Graph.Inductive.ULGraph.Query.DFS
import Data.List(partition, tails)
import Data.Ord(comparing)
import qualified Data.Set as S
import Text.PrettyPrint

data LR nt = LR nt
	   | LRTail nt
    deriving (Eq, Ord, Show)

removeLeftRecursion :: forall cfg t nt
		    . (Cfg cfg t nt, Ord nt, Ord t)
		    => cfg t nt -> FreeCfg t (LR nt)
removeLeftRecursion cfg = (toCfg . removeLR . map wrap . fromCfg) cfg
    where
    sccs :: [SCComp Gr nt (Item t nt)]
    sccs = S.toList $ leftRecScc cfg

    fromCfg = productions

    wrap :: Production t nt -> Production t (LR nt)
    wrap (nt, vs) = (LR nt, bimapVs id LR vs)

    removeLR :: [Production t (LR nt)] -> [Production t (LR nt)]
    removeLR prods = foldl f prods sccs
	where
	f :: [Production t (LR nt)]
	  -> SCComp Gr nt (Item t nt)
	  -> [Production t (LR nt)]
	f ps comp = case comp of
	    Singleton _ -> ps
	    SelfLoop nt _ -> removeDirectLeftRecursion nt ps
	    SCComp gr -> removeIndirectLeftRecursion (nodes gr) ps

    toCfg :: [Production t (LR nt)] -> FreeCfg t (LR nt)
    toCfg prods = FreeCfg {
	nonterminals' = S.map LR (nonterminals cfg)
			    `S.union` (S.fromList $ map fst prods),
	terminals' = terminals cfg,
	productionRules' = prodRules',
	startSymbol' = LR $ startSymbol cfg
	}
	where
	prodRules' nt = S.fromList [ rhs | (nt', rhs) <- prods,
					   nt' == nt ]

removeDirectLeftRecursion :: forall t nt
			  . Eq nt
			  => nt
			  -> [Production t (LR nt)]
			  -> [Production t (LR nt)]
removeDirectLeftRecursion nt ps = rest ++ fixed
    where
    ntHeaded :: Production t (LR nt) -> Bool
    ntHeaded (nt', _) = nt' == LR nt

    (ntHeads, rest) = partition ntHeaded ps

    mkRhs :: Production t (LR nt) -> Either (Vs t (LR nt)) (Vs t (LR nt))
    mkRhs (_, NT (LR nt') : vs)
	| nt == nt'  = Left (vs ++ [NT $ LRTail nt])
    mkRhs (_, vs) = Right (vs ++ [NT $ LRTail nt])

    ntRhss, ntTailRhss :: [Vs t (LR nt)]
    (ntTailRhss, ntRhss) = partitionEithers $ map mkRhs ntHeads

    fixed = map mkNTProd ntRhss ++ baseProd : map mkNTTailProd ntTailRhss
	where
	mkNTProd rhs = (LR nt, rhs)
	mkNTTailProd rhs = (LRTail nt, rhs)
	baseProd = (LRTail nt, [])

removeIndirectLeftRecursion :: forall nt t
			    . S.Set nt
			    -> [Production t (LR nt)]
			    -> [Production t (LR nt)]
removeIndirectLeftRecursion nts prods = foldl f prods nts'
    where
    nts' :: [[nt]]
    nts' = tails $ S.toList nts

    f :: [Production t (LR nt)] -> [nt] -> [Production t (LR nt)]
    f prods [] = prods
    f prods (n : ns) = error "removeIndirectLeftRecursion.f"

items :: forall t nt . (nt -> Bool) -> Production t nt -> [Item t nt]
items isNullable prod = go $ mkInitialItem prod
    where
    go :: Item t nt -> [Item t nt]
    go item = case nextV item of
	Just (NT nt) -> if isNullable nt
	    then item : maybe [] go (nextItem item)
	    else [item]
	_ -> []

type E t nt = (nt, nt, Item t nt)

-- | Is the grammar left-recursive?
isLeftRecursive :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> Bool
isLeftRecursive cfg = case S.toList $ leftRecScc cfg of
			 SCComp _ : _ -> True
			 SelfLoop _ _ : _ -> True
			 _ -> False

-- | Produces a pretty-printed report giving the left-recursion of the
-- grammar.
reportLeftRec :: (Cfg cfg t nt, Ord nt, Ord t)
	      => (V t nt -> Doc)
	      -> cfg t nt
	      -> Doc
reportLeftRec pv = leftRecReport pv . leftRecScc

leftRecReport :: forall t nt
	      . (V t nt -> Doc)
	      -> S.Set (SCComp Gr nt (Item t nt))
	      -> Doc
leftRecReport prettyV = vcat .	map f . S.toList
    where
    prettyNT = prettyV . NT

    f :: SCComp Gr nt (Item t nt) -> Doc
    f (Singleton _) = empty
    f (SelfLoop n es) = text "direct left-recursion on" <+> prettyNT n <+> text "via"
			    $$ nest 4 items'
	where
	items' = vcat [cpretty e prettyV | e <- S.toList es]
    f (SCComp gr) = text "indirect left-recursion on" <+> hsep (map prettyNT ns)
			$$ nest 4 es'
	where
	ns = S.toList $ nodes gr
	es = edges gr
	es' = vcat $ map g es
	g :: Edge nt (Item t nt) -> Doc
	g (src, dst, item) = hsep [prettyNT src,
				   arrow,
				   cpretty item prettyV,
				   arrow,
				   prettyNT dst]
	arrow = text "->"

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

leftRecScc :: forall cfg nt t
	   . (Cfg cfg t nt, Ord nt, Ord t)
	   => cfg t nt
	   -> S.Set (SCComp Gr nt (Item t nt))
leftRecScc cfg = S.fromList $ map categorizeScc scc'
    where
    categorizeScc :: [nt] -> SCComp Gr nt (Item t nt)
    categorizeScc [] = error "leftRecScc.categorizeScc [] : impossible"
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
    gr = makeLeftRecGraph cfg

    hasSelfLoop :: nt -> Bool
    hasSelfLoop n = n `elem` suc gr n

makeLeftRecGraph :: (Cfg cfg t nt, Ord nt)
                 => cfg t nt -> ULGraph Gr nt (Item t nt)
makeLeftRecGraph = mkULGraph [] . makeEdges

makeEdges :: forall cfg t nt
          . (Cfg cfg t nt, Ord nt)
          => cfg t nt -> [E t nt]
makeEdges cfg = map itemEdge allItems
    where
    allItems :: [Item t nt]
    allItems = do
        (nt, rhs) <- productions cfg
        items isNullable (nt, rhs)

    itemEdge :: Item t nt -> E t nt
    itemEdge item = (hdNode, ntNode, item)
        where
        hdNode = fst $ production item
        ntNode = nt
            where
            Just (NT nt) = nextV item

    isNullable :: nt -> Bool
    isNullable nt = nt `S.member` nullables cfg
