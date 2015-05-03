{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.CyclicTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.Cyclic
import Data.Cfg.EpsilonProductions(removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg, toFreeCfg)
import Data.Cfg.Instances()
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.Cyclic" [
    cyclicDetectionTest,
    cyclicProp
    ]

cyclicProp :: Test
cyclicProp = testProperty "result of removeCycles is not cyclic" f
    where
    f :: FreeCfg Int Int -> Bool
    f = not . isCyclic . removeCycles . removeEpsilonProductions

cyclicDetectionTest :: Test
cyclicDetectionTest = testCase "cyclic detection" $ do
    assertBool "cyclic is cyclic"
	$ isCyclic cyclic
    assertBool "sneakyCyclic is cyclic"
	$ isCyclic sneakyCyclic
    assertBool "noncyclic is not cyclic"
	$ (not . isCyclic) noncyclic

cyclic :: FreeCfg String String
cyclic = toFreeCfg [bnf|
    a ::= b.
    b ::= c.
    c ::= a | D.
    |]

sneakyCyclic :: FreeCfg String String
sneakyCyclic = toFreeCfg [bnf|
    a ::= b.
    b ::= c d.
    c ::= a | D.
    d ::= .
    |]

noncyclic :: FreeCfg String String
noncyclic = toFreeCfg [bnf|
    a ::= b.
    b ::= c.
    c ::= A | D.
    |]

