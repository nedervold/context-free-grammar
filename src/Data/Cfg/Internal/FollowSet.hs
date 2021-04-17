-- | Follow sets of a context-free grammar.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Internal.FollowSet(followSetMap) where

import Control.Monad(guard)
import Data.Cfg.Augment
import Data.Cfg.Cfg
import Data.Cfg.Collect(collectOnFirst)
import Data.Cfg.Internal.FirstSet(firstsOfVs)
import Data.Cfg.FixedPoint(fixedPoint)
import Data.List(tails)
import Data.Cfg.LookaheadSet hiding(unions)
import qualified Data.Cfg.LookaheadSet as LA
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Represents the environment following a nonterminal symbol.  A
-- production @foo ::= <vs> bar <vs'>@ will contribute a 'FollowSite' record
-- with @ntTail == <vs'>@ and @prodHead == foo@, where @<vs>@ is a
-- (possibly empty) list of vocabulary symbols.
data FollowSite t nt = FollowSite {
    ntTail :: AugVs t nt,
    prodHead :: AugNT nt
    }

-- | Calculates a map that gives all the follow sites in the grammar
-- for the given nonterminal.
followSitesMap :: (Cfg cfg (AugT t) (AugNT nt), Ord nt)
      => cfg (AugT t) (AugNT nt)
      -> M.Map (AugNT nt) [FollowSite t nt]
followSitesMap cfg = M.fromList . collectOnFirst $ do
    prodHd <- S.toList $ nonterminals cfg
    let rhss = S.toList $ productionRules cfg prodHd
    guard (not $ null rhss)
    rhs <- rhss
    NT nt : tl <- tails rhs
    return (nt, FollowSite { ntTail = tl, prodHead = prodHd })

-- | Given what we know of firsts and follows, find the first set of a
-- follow site.
firstsOfFollowSite :: forall t nt . (Ord t, Ord nt)
                   => (AugNT nt -> LookaheadSet t)
                   -> M.Map (AugNT nt) (LookaheadSet t)
                   -> FollowSite t nt
                   -> LookaheadSet t
firstsOfFollowSite firsts knownFollows followSite
    = firstsOfNTTail <> firstsOfProdHead
    where
    firstsOfNTTail, firstsOfProdHead  :: LookaheadSet t
    firstsOfNTTail = firstsOfVs firsts (ntTail followSite)
    -- firstsOfProdHead = knownFollows M.! prodHead followSite
    firstsOfProdHead = M.findWithDefault err (prodHead followSite) knownFollows
        where
        err = error "firstsOfFollowSite.firstsOfProdHead"

-- | Returns the follow sets for the grammar as a map.
followSetMap :: forall cfg t nt
             . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t)
             => cfg (AugT t) (AugNT nt)
                     -- ^ the grammar
             -> (AugNT nt -> LookaheadSet t)
                     -- ^ 'firstSet' for the grammar
             -> M.Map (AugNT nt) (LookaheadSet t)
followSetMap cfg fs = fixedPoint go initMap
    where
    go :: M.Map (AugNT nt) (LookaheadSet t)
       -> M.Map (AugNT nt) (LookaheadSet t)
    go oldFols = M.mapWithKey (\ k v -> LA.unions $ f k v) oldFols
        where
        f :: AugNT nt -> LookaheadSet t -> [LookaheadSet t]
        f nt oldFollows = oldFollows : map (firstsOfFollowSite fs oldFols) folSites
            where
            folSites = M.findWithDefault [] nt followSitesMap'

    initMap :: M.Map (AugNT nt) (LookaheadSet t)
    initMap = M.fromList [(nt, case nt of
                                   StartSymbol -> singleton EOF
                                   _ -> empty) | nt <- nts]
        where
        nts = S.toList $ nonterminals cfg

    followSitesMap' :: M.Map (AugNT nt) [FollowSite t nt]
    followSitesMap' = followSitesMap cfg
