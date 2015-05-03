-- | Functionality for detecting and removing left-recursion.
{-# LANGUAGE FlexibleContexts #-} -- for Pretty (V t nt)
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftRecursion(
    LR(..),
    SCComp(..),
    isLeftRecursive,
    -- removeLeftRecursion,
    reportLeftRec) where

import Data.Cfg.Cfg
import Data.Cfg.Item
import Data.Cfg.LeftRecursion.Cycles
import Data.Cfg.Pretty
import Data.Cfg.SCComp(SCComp(..))
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.ULGraph hiding (empty)
import qualified Data.Set as S
import Text.PrettyPrint

-- | Nonterminal wrapper to introduce symbols for tails of directly
-- recursive productions.
data LR nt = LR nt	-- ^ wrapped original symbols
	   | LRTail nt	-- ^ tail symbols
    deriving (Eq, Ord, Show)

-- | Is the grammar left-recursive?
isLeftRecursive :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> Bool
isLeftRecursive cfg = case S.toList $ lrSccs cfg of
			 SCComp _ : _ -> True
			 SelfLoop _ _ : _ -> True
			 _ -> False

------------------------------------------------------------

-- | Produces a pretty-printed report giving the left-recursion of the
-- grammar.
reportLeftRec :: forall cfg t nt
	      . (Cfg cfg t nt, Ord nt, Ord t,
		  Pretty (V t nt), Pretty (Item t nt))
	      => cfg t nt
	      -> Doc
reportLeftRec = vcat . map f . S.toList . lrSccs
    where
    prettyNT = pretty . (NT :: nt -> V t nt)

    f :: SCComp Gr nt (Item t nt) -> Doc
    f (Singleton _) = empty
    f (SelfLoop n es) = text "direct left-recursion on" <+> prettyNT n <+> text "via"
			    $$ nest 4 items'
	where
	items' = vcat [pretty e | e <- S.toList es]
    f (SCComp gr) = text "indirect left-recursion on" <+> hsep (map prettyNT ns)
			$$ nest 4 es'
	where
	ns = S.toList $ nodes gr
	es = edges gr
	es' = vcat $ map g es
	g :: Edge nt (Item t nt) -> Doc
	g (src, dst, item) = hsep [prettyNT src,
				   arrow,
				   pretty item,
				   arrow,
				   prettyNT dst]
	arrow = text "->"


{-
-- | Removes left recursion from the grammar.
removeLeftRecursion :: forall cfg t nt
		    . (Cfg cfg t nt, Ord nt, Ord t)
		    => cfg t nt -> FreeCfg t (LR nt)
removeLeftRecursion cfg = (toCfg . removeLR . map wrap . fromCfg) cfg
    where
    sccs :: [SCComp Gr nt (Item t nt)]
    sccs = S.toList $ lrSccs cfg

    fromCfg = productions

    wrap :: Production t nt -> Production t (LR nt)
    wrap (Production nt vs) = Production (LR nt) (bimapVs id LR vs)

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
			    `S.union` S.fromList (map productionHead prods),
	terminals' = terminals cfg,
	productionRules' = prodRules',
	startSymbol' = LR $ startSymbol cfg
	}
	where
	prodRules' = S.fromList . flip lookupProductions prods

removeIndirectLeftRecursion :: forall nt t
			    . (Eq nt, Eq t)
			    => S.Set nt
			    -> [Production t (LR nt)]
			    -> [Production t (LR nt)]
removeIndirectLeftRecursion nts prods = foldl (flip $ uncurry f) prods nts'
    where
    nts' :: [(nt, [nt])]
    nts' = map split $ tail $ reverse $ tails $ S.toList nts
	where
	split nts'' = (head nts'', tail nts'')

    -- The mechanism above is just to implement the nested i/j loop
    -- in the standard imperative algorithm:
    --
    -- for i in [1..n]
    --	   for j in [1..i-1]
    --	      expand A_j in A_i ::= A_j alpha.
    --	   removeDirectRecursion in A_i

    f :: nt -> [nt] -> [Production t (LR nt)] -> [Production t (LR nt)]
    f nt prevNTs prods' = removeDirectLeftRecursion nt
			      $ inlinePrevNts nt prevNTs prods'

    inlinePrevNts :: nt -> [nt] -> [Production t (LR nt)]
				-> [Production t (LR nt)]
    inlinePrevNts nt prevNTs prods' = do
	Production hd rhs <- prods'
	if hd == LR nt
		     && not (null rhs)
			 && head rhs `elem` map (NT . LR) prevNTs
	    then do
		let NT prevNT = head rhs
		rhs' <- lookupProductions prevNT prods'
		return $ Production hd (rhs' ++ tail rhs)
	    else return $ Production hd rhs

removeDirectLeftRecursion :: forall t nt
			  . Eq nt
			  => nt
			  -> [Production t (LR nt)]
			  -> [Production t (LR nt)]
removeDirectLeftRecursion nt ps = if null ntTailRhss
                                      then ps
                                      else rest ++ fixed
    where
    (ntHeads, rest) = partitionProds (LR nt) ps

    mkRhs :: Production t (LR nt) -> Either (Vs t (LR nt)) (Vs t (LR nt))
    mkRhs (Production _ (NT (LR nt') : vs))
        | nt == nt'  = Left (vs ++ [NT $ LRTail nt])
    mkRhs (Production _ vs) = Right (vs ++ [NT $ LRTail nt])

    ntRhss, ntTailRhss :: [Vs t (LR nt)]
    (ntTailRhss, ntRhss) = partitionEithers $ map mkRhs ntHeads

    fixed = map mkNTProd ntRhss ++ baseProd : map mkNTTailProd ntTailRhss
        where
        mkNTProd = Production (LR nt)
        mkNTTailProd = Production (LRTail nt)
        baseProd = Production (LRTail nt) []

partitionProds :: (Eq nt)
               => nt -> [Production t nt]
                     -> ([Production t nt], [Production t nt])
partitionProds nt = partition $ \ (Production nt' _) -> nt == nt'
-}
