{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.ReachableTests (
    tests
    ) where

import Data.Cfg.Bnf(bnf)
import Data.Cfg.Cfg(Cfg(..))
import Data.Cfg.FreeCfg(FreeCfg(..), toFreeCfg)
import Data.Cfg.Instances()
import Data.Cfg.Reachable
import Data.Cfg.TestGrammars(assertEqCfg', wiki)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertEqual)

tests :: Test
tests = testGroup "Data.Cfg.Reachable" [
    wikiTest,
    removeUnreachablesProp
    ]

wikiTest :: Test
wikiTest = testCase "wiki reachability test" $ do
    assertEqual "reachables" (S.fromList $ words "s b c e")
                             (reachables wiki)
    assertEqual "unreachables" (S.fromList $ words "d")
                               (unreachables wiki)
    assertEqCfg' "reachability" expected $ removeUnreachables wiki

    where
    reach = [bnf|
        s ::= b B | c C | e E.
        b ::= b B | B.
        c ::= c C | C.
        e ::= e E.
        |]

    expected :: FreeCfg String String
    expected = (toFreeCfg reach){
            terminals' = terminals wiki
        }

removeUnreachablesProp :: Test
removeUnreachablesProp = testProperty "removeUnreachables does" f
    where
    f :: FreeCfg Int Int -> Bool
    f = S.null . unreachables . removeUnreachables
