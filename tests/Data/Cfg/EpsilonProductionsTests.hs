{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Cfg.EpsilonProductionsTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.Cfg
import Data.Cfg.EpsilonProductions
import Data.Cfg.FreeCfg(FreeCfg, toFreeCfg)
import Data.Cfg.TestGrammars(assertEqCfg, epsProds)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)
import Text.PrettyPrint(Doc, text)

tests :: Test
tests = testGroup "Data.Cfg.EpsilonProductions" [
    epsilonProductionsTest
    ]

epsilonProductionsTest :: Test
epsilonProductionsTest = testCase "removal of eps-productions" $ do
    assertEqCfg ctxt ctxt
	"removed epsilon-productions correctly"
	expected actual
    assertBool "removed epsilon-productions completely"
	(isEpsilonFree expected)

    where
    actual, expected :: FreeCfg String String
    actual = removeEpsilonProductions $ toFreeCfg epsProds
    expected = toFreeCfg [bnf|
	b ::= Z | a Z | Z a | a Z a .
	a ::= A .
	|]

    ctxt :: V String String -> Doc
    ctxt v = text $ case v of
                 NT nt -> nt
                 T t -> t
