{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.CfgTests (sampleCfg, tests) where

import Control.Monad(forM)
import Data.Char(toLower, toUpper)
import Data.Cfg(Cfg'(..), V(..), cpretty)
import qualified Data.Cfg.GramTests
import qualified Data.Map as M
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.QuickCheck
import Text.PrettyPrint

instance Arbitrary (Cfg' Int Int) where
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
	    return $ (nt, S.fromList rhss)

	let map' = M.fromList pairs
	return $ Cfg' {
	    nonterminals' = S.fromList nts,
	    terminals' = S.fromList ts,
	    productionRules' = (map' M.!),
	    startSymbol' = 0
	    }

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

pretty :: Cfg' Int Int -> Doc
pretty cfg = cpretty cfg ctxt

sampleCfg :: IO ()
sampleCfg = do
    cfgs <- sample' (arbitrary :: Gen (Cfg' Int Int))
    mapM_ (print . pretty) (take 3 cfgs)

tests :: Test
tests = testGroup "Cfg tests" [
    Data.Cfg.GramTests.tests
    ]