module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Data.Cfg.Cyclic(removeCycles')
import Data.Cfg.EpsilonProductions(EP, removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.LeftRecursion(LR, isLeftRecursive, removeLeftRecursionBounded)
import Data.Cfg.TestGrammars(g0, leftRec)
import Data.Maybe(fromJust, isNothing)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.QuickCheck.Property(Property, (==>))
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    leftRecursionDetectionTest,
    leftRecursionProp
    ]

leftRecursionProp :: Test
leftRecursionProp
    = testProperty "result of removeLeftRecursion is not left-recursive" f
    where
    f :: FreeCfg Int Int -> Property
    f cfg = not itExploded ==> not (isLeftRecursive cfg')
	where
	mCfg = rmLR cfg
	itExploded = isNothing mCfg
	cfg' = fromJust mCfg

    rmLR :: FreeCfg Int Int -> Maybe (FreeCfg Int (LR (EP Int)))
    rmLR = removeLeftRecursionBounded 5000
	       . removeCycles'
		   . removeEpsilonProductions

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
	$ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
        $ (not . isLeftRecursive) g0
