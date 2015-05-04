module Data.Cfg.LeftFactorTests (
    tests
    ) where

import Data.Cfg.LeftFactor(hasLeftFactors)
import Data.Cfg.TestGrammars(micro, micro')
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.LeftFactor" [
    leftFactorDetectionTest
    ]

leftFactorDetectionTest :: Test
leftFactorDetectionTest = testCase "left-factors detection" $ do
    assertBool "micro' grammar's left factors are detected"
	$ hasLeftFactors micro'
    assertBool "micro grammar's lack of left factors is detected"
	$ (not . hasLeftFactors) micro

{-
    leftFactorProp
    ]

leftFactorsProp :: Test
leftFactorsProp
    = testProperty "result of removeLeftFactors has no left factors" f
    where
    f :: FreeCfg Int Int -> Bool
    f = hasLeftFactors . removeLeftFactors
-}

