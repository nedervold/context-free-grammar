module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Data.Cfg.LeftRecursion(isLeftRecursive)
import Data.Cfg.TestGrammars(g0, leftRec)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    leftRecursionDetectionTest
    ]

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
	$ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
	$ (not . isLeftRecursive) g0
