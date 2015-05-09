-- | Left factoring of grammars.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.LeftFactor (
    LF(..),
    hasLeftFactors,
    leftFactor
    ) where

import Control.Monad.Writer(Writer, liftM, runWriter, tell)
import Data.Cfg.Cfg(Cfg, ProductionMap, V(..), Vs, productionMap)
import Data.Cfg.FreeCfg(FreeCfg, bimapCfg, withProductionMap)
import Data.List(nub, partition)
import qualified Data.Map as M
import qualified Data.Set as S

-- | 'True' if the grammar has multiple productions of the same
-- nonterminal whose right-hand sides start with the same vocabulary
-- element.
hasLeftFactors :: (Cfg cfg t nt, Ord nt, Ord t)
	       => cfg t nt -> Bool
hasLeftFactors cfg = any hasLeftFactorsRhss (M.elems $ productionMap cfg)

hasLeftFactorsRhss :: forall nt t
		   . (Ord nt, Ord t)
		   => S.Set (Vs t nt) -> Bool
hasLeftFactorsRhss rhsSet = S.size rhsSet /= S.size (S.map safeHead rhsSet)
    where
    safeHead :: [a] -> Maybe a
    safeHead as = case as of
		      [] -> Nothing
		      a : _ -> Just a

-- | Nonterminal wrapper to introduce symbols for tails of
-- left-factored productions.
data LF t nt = LF nt
	     | LFTail nt (Vs t (LF t nt))
	     deriving (Eq, Ord, Show)

mkLFHead :: LF t nt -> Vs t (LF t nt) -> LF t nt
mkLFHead (LF base) seen = if null seen
			      then LF base
			      else LFTail base seen
mkLFHead (LFTail base vs) seen = LFTail base (vs ++ seen)

type P t nt = (LF t nt, S.Set (Vs t (LF t nt)))
type PT t nt = PatriciaTrie (V t (LF t nt)) ()

data Trie ke v = Trie (Maybe v) (M.Map ke (Trie ke v))
type PatriciaTrie ke v = Trie [ke] v

isCap :: Trie k () -> Bool
isCap (Trie (Just ()) m) = M.null m
isCap _ = False

-- | Factor out common leading prefixes to production right-hand sides.
leftFactor :: forall cfg nt t
	   . (Cfg cfg t nt, Ord nt, Ord t)
	   => cfg t nt -> FreeCfg t (LF t nt)
leftFactor = withProductionMap f . bimapCfg id LF
    where
    f :: ProductionMap t (LF t nt) -> ProductionMap t (LF t nt)
    f = M.fromList . concatMap f' . M.toList

    f' :: P t nt -> [P t nt]
    f' (hd, tls) = (hd, S.fromList alts) : ps
	 where
	 pt :: PatriciaTrie (V t (LF t nt)) ()
	 pt = patriciaTrieFromList [(vs, ()) | vs <- S.toList tls ]

	 (alts, ps) = runWriter $ patriciaTrieToAlts hd pt

type W t nt = Writer [P t nt]

patriciaTrieToAlts :: (Ord nt, Ord t)
		   => LF t nt -> PT t nt -> W t nt [Vs t (LF t nt)]
patriciaTrieToAlts hd (Trie Nothing trieMap) = trieMapToAlts hd trieMap
patriciaTrieToAlts hd (Trie (Just ()) trieMap)
    = liftM (++[[]]) $ trieMapToAlts hd trieMap

trieMapToAlts :: (Ord nt, Ord t)
	      => LF t nt
	      -> M.Map (Vs t (LF t nt)) (Trie (Vs t (LF t nt)) ())
	      -> W t nt [Vs t (LF t nt)]
trieMapToAlts hd trieMap = if M.null trieMap
    then return []
    else mapM (segTrieToAlt hd) $ M.toList trieMap

segTrieToAlt :: (Ord nt, Ord t)
	     => LF t nt
	     -> (Vs t (LF t nt), Trie (Vs t (LF t nt)) ())
	     -> W t nt (Vs t (LF t nt))
segTrieToAlt hd (seg, trie) = if isCap trie
    then return seg
    else do
	let hd' = mkLFHead hd seg
	alts <- patriciaTrieToAlts hd' trie
	tell [(hd', S.fromList alts)]
	return (seg ++ [NT hd'])


------------------------------------------------------------

patriciaTrieFromList :: Ord ke => [([ke], v)] -> PatriciaTrie ke v
patriciaTrieFromList [] = Trie Nothing M.empty
patriciaTrieFromList (kvs :: [([ke], v)]) = Trie mV subtries'
    where
    nulls, fulls :: [([ke], v)]
    (nulls, fulls) = partition (null . fst) kvs

    mV :: Maybe v
    mV = if null nulls
	     then Nothing
	     else Just (last $ map snd nulls)

    trieKeys :: Eq ke => [ke]
    trieKeys = nub $ map (head . fst) fulls

    groups :: [[([ke], v)]]
    groups = map f trieKeys
	where
	f ini = [ full | full <- fulls, head (fst full) == ini ]

    prefixGroups :: [([ke], [([ke], v)])]
    prefixGroups = map f groups
	where
	f :: [([ke], v)] -> ([ke], [([ke], v)])
	f gp = (pref, [(suffix, v) | (ke, v) <- gp,
					let suffix = drop len ke ])
	    where
	    pref = commonPrefix (map fst gp)
	    len = length pref

    subtries' :: M.Map [ke] (PatriciaTrie ke v)
    subtries' = M.fromList ls
	where
	ls :: [([ke], PatriciaTrie ke v)]
	ls = map f prefixGroups

	f :: ([ke], [([ke], v)]) -> ([ke], PatriciaTrie ke v)
	f (ini, dat) = (ini, patriciaTrieFromList dat)

commonPrefix :: Eq x => [[x]] -> [x]
commonPrefix [] = error "commonPrefix: null list"
commonPrefix xss = map head $ takeWhile allSame (zipN xss)
    where
    allSame [] = error "allSame: null list"
    allSame (x : xs) = all (==x) xs

zipN :: [[x]] -> [[x]]
zipN xss = if null xss || any null xss
               then []
               else map head xss : zipN (map tail xss)
