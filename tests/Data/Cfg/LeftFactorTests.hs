module Data.Cfg.LeftFactorTests (
    tests
    ) where

import Data.Cfg.FreeCfg(FreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.LeftFactor(hasLeftFactors, leftFactor)
import Data.Cfg.TestGrammars(micro, micro')
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)
import Test.Framework.Providers.QuickCheck2(testProperty)

tests :: Test
tests = testGroup "Data.Cfg.LeftFactor" [
    leftFactorDetectionTest,
    leftFactorProp
    ]

leftFactorDetectionTest :: Test
leftFactorDetectionTest = testCase "left-factors detection" $ do
    assertBool "micro' grammar's left factors are detected"
        $ hasLeftFactors micro'
    assertBool "micro grammar's lack of left factors is detected"
        $ (not . hasLeftFactors) micro

leftFactorProp :: Test
leftFactorProp
    = testProperty "result of leftFactor has no left factors" f
    where
    f :: FreeCfg Int Int -> Bool
    f = not . hasLeftFactors . leftFactor
