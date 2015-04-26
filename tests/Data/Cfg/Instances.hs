{-# LANGUAGE FlexibleInstances #-} -- because "Int Int" isn't "t nt"
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Cfg.Instances(ProductiveCfg(..), ProductiveEpsFreeCfg(..)) where

import Control.Monad(forM, liftM)
import Data.Char(toLower, toUpper)
import Data.Cfg.Augment(AugNT(..), AugT(..))
import Data.Cfg.Cfg(V(..))
import Data.Cfg.FreeCfg(FreeCfg(..))
import Data.Cfg.EpsilonProductions(EP(..), removeEpsilonProductions)
import Data.Cfg.LeftRecursion(LR(..))
import Data.Cfg.Pretty(Pretty(..))
import Data.Cfg.Productive(removeUnproductivesUnsafe)
import qualified Data.Map as M
import qualified Data.Set as S
import Test.QuickCheck(Arbitrary(..), Gen,
    choose, elements, listOf, suchThat, vectorOf)
import Text.PrettyPrint

instance Pretty (V Int Int) where
    pretty v = text $ map f $ base26 n
	where
	(f, n) = case v of
		     NT n' -> (toLower, n')
		     T n' -> (toUpper, n')

base26 :: Int -> String
base26 n'
    | n' < 26	= [digitToChar n']
    | otherwise = if msds == 0
		      then [digitToChar lsd]
		      else base26 msds ++ [digitToChar lsd]
    where
    (msds, lsd) = n' `divMod` 26

    digitToChar :: Int -> Char
    digitToChar digit = toEnum (fromEnum 'a' + digit)

instance Show (FreeCfg Int Int) where
    show = show . pretty

instance Arbitrary (FreeCfg Int Int) where
    arbitrary = do
	tCnt <- choose (1, 25)
	let ts = [0..tCnt-1]
	ntCnt <- choose (1, 100)
	let nts = [0..ntCnt-1]
	let vs = map T ts ++ map NT nts
	let genV = elements vs
	let genVs = listOf genV
	pairs <- forM nts $ \nt -> do
	    altCnt <- choose (1, 5)
	    rhss <- vectorOf altCnt genVs
	    return (nt, S.fromList rhss)

	let map' = M.fromList pairs
	return FreeCfg {
	    nonterminals' = S.fromList nts,
	    terminals' = S.fromList ts,
	    productionRules' = (map' M.!),
	    startSymbol' = 0
	    }

------------------------------------------------------------

instance Pretty (V Int (EP Int)) where
    pretty v = text $ f $ base26 n
	where
	(f, n) = case v of
		     NT n' -> case n' of
			 EP n'' -> (map toLower, n'')
			 EPStart n'' -> (toLowerStart, n'')
		     T n' -> (map toUpper, n')
	toLowerStart :: String -> String
	toLowerStart = (++ "$start") . map toLower

instance Show (FreeCfg Int (EP Int)) where
    show = show . pretty

------------------------------------------------------------

-- | A wrapper around a 'FreeCfg' that is guaranteed to be productive
newtype ProductiveCfg = ProductiveCfg {
    unProductiveCfg :: FreeCfg Int Int
    }

instance Show ProductiveCfg where
    show = show . unProductiveCfg

instance Arbitrary ProductiveCfg where
    arbitrary = do
	let arbitrary' = arbitrary :: Gen (FreeCfg Int Int)
	cfg <- liftM removeUnproductivesUnsafe arbitrary'
		   `suchThat` hasProductives
	let nts = nonterminals' cfg
	return $ ProductiveCfg
		   $ if startSymbol' cfg `S.member` nts
			 then cfg
			 else cfg {
			     startSymbol' = head $ S.toList nts
			     }
	where
	hasProductives :: FreeCfg Int Int -> Bool
	hasProductives = not . S.null . nonterminals'

------------------------------------------------------------

-- | A wrapper around a 'FreeCfg' that is guaranteed to be productive
-- and epsilon-production free
newtype ProductiveEpsFreeCfg = ProductiveEpsFreeCfg {
    unProductiveEpsFreeCfg :: FreeCfg Int (EP Int)
    }

instance Show ProductiveEpsFreeCfg where
    show = show . unProductiveEpsFreeCfg

instance Arbitrary ProductiveEpsFreeCfg where
    arbitrary = liftM (ProductiveEpsFreeCfg
			   . removeEpsilonProductions
			       . unProductiveCfg) arbitrary

------------------------------------------------------------

instance Pretty (V String String) where
    pretty v = text $ case v of
			  NT nt -> nt
			  T t -> t

instance Pretty (V String (EP String)) where
    pretty v = text $ case v of
			  NT (EP nt) -> nt
			  NT (EPStart nt) -> nt ++ "_start"
			  T t -> t

instance Pretty (V String (LR String)) where
    pretty v = text $ case v of
                          NT (LR nt) -> nt
                          NT (LRTail nt) -> nt ++ "_tail"
                          T t -> t

instance Pretty (V (AugT String) (AugNT String)) where
    pretty v = text $ case v of
                          NT nt -> case nt of
                              StartSymbol -> "$start"
                              AugNT s -> s
                          T t -> case t of
                              EOF -> "$EOF"
                              AugT s -> s
