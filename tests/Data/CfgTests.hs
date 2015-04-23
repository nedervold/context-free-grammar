module Data.CfgTests (tests) where

import qualified Data.Cfg.BnfTests
import qualified Data.Cfg.EpsilonProductionsTests
import qualified Data.Cfg.FirstSetTests
import qualified Data.Cfg.FollowSetTests
import qualified Data.Cfg.FreeCfgTests
import qualified Data.Cfg.LeftRecursionTests
import qualified Data.Cfg.LookaheadSetTests
import qualified Data.Cfg.ProductiveTests
import qualified Data.Cfg.ReachableTests
import Test.Framework(Test, testGroup)

tests :: Test
tests = testGroup "Data.Cfg" [
    Data.Cfg.BnfTests.tests,
    Data.Cfg.FirstSetTests.tests,
    Data.Cfg.EpsilonProductionsTests.tests,
    Data.Cfg.FollowSetTests.tests,
    Data.Cfg.FreeCfgTests.tests,
    Data.Cfg.LeftRecursionTests.tests,
    Data.Cfg.LookaheadSetTests.tests,
    Data.Cfg.ProductiveTests.tests,
    Data.Cfg.ReachableTests.tests
    ]
