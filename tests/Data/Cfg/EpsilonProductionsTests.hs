{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.EpsilonProductionsTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.EpsilonProductions
import Data.Cfg.FreeCfg(FreeCfg(..), bimapCfg, toFreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.Productive(removeUnproductives)
import Data.Cfg.Reachable(removeUnreachables)
import Data.Cfg.TestGrammars(assertEqCfg', epsProds)
import Data.Maybe(fromJust, isJust)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertBool)
import Test.QuickCheck((==>), Property)

tests :: Test
tests = testGroup "Data.Cfg.EpsilonProductions" [
    epsilonProductionsTest,
    epsilonProductionsProp
    ]

epsilonProductionsProp :: Test
epsilonProductionsProp
    = testProperty "result of removeEpsilonProductions is epsilon-free" f
    where
    f :: FreeCfg Int Int -> Property
    f cfg = isJust mcfg' ==> isEpsilonFree $ fromJust mcfg'
	where
	mcfg' = removeEpsilonProductions' cfg

    removeEpsilonProductions' :: FreeCfg Int Int
			      -> Maybe (FreeCfg Int (EP Int))
    removeEpsilonProductions' cfg
	= fmap removeEpsilonProductions
	      $ removeUnproductives
		  $ removeUnreachables cfg

epsilonProductionsTest :: Test
epsilonProductionsTest = testCase "removal of eps-productions" $ do
    assertEqCfg'
	"removed epsilon-productions correctly"
	expected actual
    assertBool "removed epsilon-productions completely"
	(isEpsilonFree expected)

    where
    actual, expected :: FreeCfg String (EP String)
    actual = removeEpsilonProductions $ toFreeCfg epsProds
    expected = bimapCfg id EP cfg
    cfg = [bnf|
        b ::= Z | a Z | Z a | a Z a .
        a ::= A .
        |]

