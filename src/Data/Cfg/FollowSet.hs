-- | Follow sets of a context-free grammar.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.FollowSet (
    followSet, followSetMap
    ) where

import Control.Monad(guard)
import Data.Cfg.Augment
import Data.Cfg.Cfg
import Data.Cfg.FirstSet(firstsOfVs)
import Data.Cfg.FixedPoint(fixedPoint)
import Data.List(nub, tails)
import Data.Cfg.LookaheadSet hiding(unions)
import qualified Data.Cfg.LookaheadSet as LA
import qualified Data.Map as M
import Data.Monoid(Monoid(..))
import qualified Data.Set as S

-- | Represents the environment following a nonterminal symbol.	 A
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

    where
    collectOnFirst :: Eq a => [(a, b)] -> [(a, [b])]
    collectOnFirst pairs = [(a, bsFor a) | a <- as]
	where
	as = nub $ map fst pairs
	bsFor a = [ b | (a', b) <- pairs, a == a' ]

-- | Functions that take a 'firstSet' value and a (possibly partial)
-- 'followSetMap' value and calculate a result.	 We use the fact that
-- @FF t nt@ is a monad.
type FF t nt res = (AugNT nt -> LookaheadSet t)
		 -> M.Map (AugNT nt) (LookaheadSet t)
		 -> res

-- | Given what we know of firsts and follows, find the first set of a
-- follow site.
firstsOfFollowSite :: forall t nt . (Ord t, Ord nt)
		   => FF t nt (FollowSite t nt -> LookaheadSet t)
firstsOfFollowSite firsts follows followSite
    = firstsOfNTTail `mappend` firstsOfProdHead
    where
    firstsOfNTTail, firstsOfProdHead  :: LookaheadSet t
    firstsOfNTTail = firstsOfVs firsts (ntTail followSite)
    firstsOfProdHead = follows M.! prodHead followSite

-- | Returns the follow set of the nonterminal for the grammar as a
-- map.
followSetMap :: forall cfg t nt
	     . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t, Show nt)
	     => cfg (AugT t) (AugNT nt)
	     -> (AugNT nt -> LookaheadSet t)
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

-- | Returns the follow set of the nonterminal for the grammar. To
-- avoid recalculations, hold a copy of @followSet cfg@.
followSet :: forall cfg t nt
	  . (Cfg cfg (AugT t) (AugNT nt), Ord nt, Ord t, Show nt)
          => cfg (AugT t) (AugNT nt)
          -> (AugNT nt -> LookaheadSet t)
          -> AugNT nt
          -> LookaheadSet t
followSet cfg fs nt = followSetMap cfg fs M.! nt