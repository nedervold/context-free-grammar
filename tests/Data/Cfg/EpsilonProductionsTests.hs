{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.EpsilonProductionsTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.EpsilonProductions(EP(..), isEpsilonFree,
    removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg(..), bimapCfg, toFreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.TestGrammars(assertEqCfg', epsProds)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.EpsilonProductions" [
    epsilonProductionsTest,
    epsilonProductionsProp
    ]

epsilonProductionsProp :: Test
epsilonProductionsProp
    = testProperty "result of removeEpsilonProductions is epsilon-free" f
    where
    f :: FreeCfg Int Int -> Bool
    f = isEpsilonFree . removeEpsilonProductions

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

