-- | Functionality for detecting and removing left-recursion.
{-# LANGUAGE FlexibleContexts #-} -- for Pretty (V t nt)
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftRecursion(
    LR(..),
    SCComp(..),
    isLeftRecursive,
    removeLeftRecursion,
    removeLeftRecursionBounded,
    reportLeftRec) where

import Control.Monad(guard)
import Data.Cfg.Cfg
import Data.Cfg.CycleRemoval(SCComp(..))
import qualified Data.Cfg.CycleRemoval as CR
import Data.Cfg.FreeCfg(FreeCfg, bimapCfg)
import Data.Cfg.Item
import Data.List(partition)
import Data.Cfg.LeftRecursion.Cycles
import Data.Cfg.Pretty
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.ULGraph hiding (empty)
import qualified Data.Map as M
import qualified Data.Set as S
import Text.PrettyPrint

-- | Nonterminal wrapper to introduce symbols for tails of directly
-- recursive productions.
data LR nt = LR nt      -- ^ wrapped original symbols
           | LRTail nt  -- ^ tail symbols
    deriving (Eq, Ord, Show)

-- | Is the grammar left-recursive?
isLeftRecursive :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> Bool
isLeftRecursive cfg = case S.toList $ lrSccs cfg of
                         SCComp _ : _ -> True
                         SelfLoop _ _ : _ -> True
                         _ -> False

-- | An equivalent grammar without left-recursion.
removeLeftRecursion :: forall cfg nt t
                    . (Cfg cfg t nt, Ord nt, Ord t)
                    => cfg t nt -> FreeCfg t (LR nt)
removeLeftRecursion cfg
    = CR.removeCycles' indirect direct (S.toList $ lrSccs cfg') cfg'
    where
    cfg' :: FreeCfg t (LR nt)
    cfg' = bimapCfg id LR cfg

    indirect :: LR nt -> LR nt -> ProductionMap t (LR nt)
                               -> ProductionMap t (LR nt)
    indirect src dst pm = if null rec'
                              then pm
                              else M.insert src (S.fromList newSrcRhss) pm
        where
        srcRhss :: [Vs t (LR nt)]
        srcRhss = S.toList $ pm M.! src

        (rec', nonrec) = partition isRec srcRhss
            where
            isRec :: Vs t (LR nt) -> Bool
            isRec rhs = case rhs of
                            NT (LR nt) : _ -> dst == LR nt
                            _ -> False

        dstRhss :: [Vs t (LR nt)]
        dstRhss = S.toList $ pm M.! dst

        newSrcRhss :: [Vs t (LR nt)]
        newSrcRhss = nonrec ++ newRec
            where
            newRec = do
                srcRhs <- rec'
                dstRhs <- dstRhss
                return (dstRhs ++ tail srcRhs)

    direct :: LR nt -> ProductionMap t (LR nt) -> ProductionMap t (LR nt)
    direct (LRTail _) _ = error "removeLeftRecursion.direct: saw LRTail"
    direct (LR nt) pm = if null rec'
                            then pm
                            else newPM `M.union` pm
        where
        rhss = S.toList $ pm M.! LR nt
        (rec', nonrec) = partition isRec rhss
            where
            isRec rhs = case rhs of
                            NT (LR nt') : _ -> nt == nt'
                            _ -> False

        newNTRhss = S.fromList $ map f nonrec
            where
            f rhs = rhs ++ [NT $ LRTail nt]
        newNTTailRhss = S.fromList ([] : map f rec')
            where
            f rhs = tail rhs ++ [NT $ LRTail nt]

        newPM :: ProductionMap t (LR nt)
        newPM = M.fromList [(LR nt, newNTRhss),
                            (LRTail nt, newNTTailRhss)]

-- | An equivalent grammar without left-recursion, if the number of
-- productions does not exceed the given limit.
removeLeftRecursionBounded :: forall cfg nt t
                           . (Cfg cfg t nt, Ord nt, Ord t)
                           => Int -> cfg t nt -> Maybe (FreeCfg t (LR nt))
removeLeftRecursionBounded maxSize cfg
    = CR.removeCyclesM' indirectM directM (S.toList $ lrSccs cfg') cfg'
    where
    cfg' :: FreeCfg t (LR nt)
    cfg' = bimapCfg id LR cfg

    pmSize :: ProductionMap t (LR nt) -> Int
    pmSize = sum . map S.size . M.elems

    checkSize :: ProductionMap t (LR nt) -> Maybe (ProductionMap t (LR nt))
    checkSize pm = do
        guard (pmSize pm > maxSize)
        return pm

    indirectM :: LR nt -> LR nt -> ProductionMap t (LR nt)
                                -> Maybe (ProductionMap t (LR nt))
    indirectM src dst pm = checkSize $ indirect src dst pm

    directM :: LR nt -> ProductionMap t (LR nt)
                     -> Maybe (ProductionMap t (LR nt))
    directM nt pm = checkSize $ direct nt pm

    indirect :: LR nt -> LR nt -> ProductionMap t (LR nt)
                               -> ProductionMap t (LR nt)
    indirect src dst pm = if null rec'
                              then pm
                              else M.insert src (S.fromList newSrcRhss) pm
        where
        srcRhss :: [Vs t (LR nt)]
        srcRhss = S.toList $ pm M.! src

        (rec', nonrec) = partition isRec srcRhss
            where
            isRec :: Vs t (LR nt) -> Bool
            isRec rhs = case rhs of
                            NT (LR nt) : _ -> dst == LR nt
                            _ -> False

        dstRhss :: [Vs t (LR nt)]
        dstRhss = S.toList $ pm M.! dst

        newSrcRhss :: [Vs t (LR nt)]
        newSrcRhss = nonrec ++ newRec
            where
            newRec = do
                srcRhs <- rec'
                dstRhs <- dstRhss
                return (dstRhs ++ tail srcRhs)

    direct :: LR nt -> ProductionMap t (LR nt) -> ProductionMap t (LR nt)
    direct (LRTail _) _ = error "removeLeftRecursion.direct: saw LRTail"
    direct (LR nt) pm = if null rec'
                            then pm
                            else newPM `M.union` pm
        where
        rhss = S.toList $ pm M.! LR nt
        (rec', nonrec) = partition isRec rhss
            where
            isRec rhs = case rhs of
                            NT (LR nt') : _ -> nt == nt'
                            _ -> False

        newNTRhss = S.fromList $ map f nonrec
            where
            f rhs = rhs ++ [NT $ LRTail nt]
        newNTTailRhss = S.fromList ([] : map f rec')
            where
            f rhs = tail rhs ++ [NT $ LRTail nt]

        newPM :: ProductionMap t (LR nt)
        newPM = M.fromList [(LR nt, newNTRhss),
                            (LRTail nt, newNTTailRhss)]


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
