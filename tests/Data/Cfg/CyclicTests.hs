{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.CyclicTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.Cyclic
import Data.Cfg.FreeCfg(FreeCfg, toFreeCfg)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.Cyclic" [
    cyclicDetectionTest
    ]

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

