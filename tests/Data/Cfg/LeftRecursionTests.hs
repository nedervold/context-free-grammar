{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Control.Monad(when)
import Data.Cfg.Bnf
import Data.Cfg.Cfg(productions)
import Data.Cfg.Cyclic(removeCycles')
import Data.Cfg.EpsilonProductions(EP, removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg, toFreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.LeftRecursion(LR, isLeftRecursive, removeLeftRecursion)
import Data.Cfg.TestGrammars(g0, leftRec)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertBool, assertFailure)

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    leftRecursionDetectionTest,
    leftRecursionTest,
    leftRecursionProp
    ]

leftRecursionTest :: Test
leftRecursionTest = testCase "left-recursion removal" $ do
    print cfg'
    when (isLeftRecursive cfg') $ assertFailure "BOOM!"

    where
    cfg' = removeLeftRecursion $ removeCycles' $ removeEpsilonProductions cfg
    cfg = toFreeCfg [bnf|
	expr ::= expr M term | term.
	term ::= term X factor | factor.
	factor ::= L expr R | I.
	|]
    {-
    cfg = toFreeCfg [bnf|
	a ::= b X Y | X.
	b ::= c d.
	c ::= a | C.
	d ::= D.
	|]
	-}

leftRecursionProp :: Test
leftRecursionProp
    = testProperty "result of removeLeftRecursion is not left-recursive" f
    where
    f :: FreeCfg Int Int -> Bool
    f = isOversizedOrIsntLeftRecursive
		. removeLeftRecursion
		    . removeCycles'
			. removeEpsilonProductions

{-
    f = not
	    . isLeftRecursive
		. removeLeftRecursion
		    . removeCycles'
			. removeEpsilonProductions
-}

    isOversizedOrIsntLeftRecursive :: FreeCfg Int (LR (EP Int)) -> Bool
    isOversizedOrIsntLeftRecursive cfg
	= isOversized cfg || not (isLeftRecursive cfg)

    isOversized :: FreeCfg t nt -> Bool
    isOversized cfg = not $ null $ drop 10000 $ productions cfg

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
        $ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
        $ (not . isLeftRecursive) g0
