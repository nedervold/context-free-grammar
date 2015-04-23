{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cfg.FreeCfgInstances() where

import Control.Monad(forM)
import Data.Cfg.Cfg(V(..), cprettyCfg)
import Data.Cfg.CPretty(CPretty(..))
import Data.Cfg.FreeCfg(FreeCfg(..))
import Data.Char(toLower, toUpper)
import qualified Data.Map as M
import qualified Data.Set as S
import Test.QuickCheck(Arbitrary(..), choose, elements, listOf, vectorOf)
import Text.PrettyPrint

instance CPretty (FreeCfg String String) (V String String -> Doc)
    where
    cpretty = cprettyCfg

------------------------------------------------------------

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

instance CPretty (FreeCfg Int Int) (V Int Int -> Doc)
    where
    cpretty = cprettyCfg

instance Show (FreeCfg Int Int) where
    show = show . pretty
	where
	ctxt :: V Int Int -> Doc
	ctxt v = text $ map f $ base26 n
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

        pretty :: FreeCfg Int Int -> Doc
        pretty cfg = cpretty cfg ctxt
