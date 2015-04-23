{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.ReachableTests (
    tests
    ) where

import Data.Cfg.Bnf(bnf)
import Data.Cfg.Cfg(Cfg(..), V(..))
import Data.Cfg.FreeCfg(FreeCfg(..), toFreeCfg)
import Data.Cfg.FreeCfgInstances()
import Data.Cfg.Reachable
import Data.Cfg.TestGrammars(assertEqCfg, wiki)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)
import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.Reachable" [
    wikiTest
    ]

wikiTest :: Test
wikiTest = testCase "wiki reachability test" $ do
    assertEqual "reachables" (S.fromList $ words "s b c e")
			     (reachables wiki)
    assertEqual "unreachables" (S.fromList $ words "d")
			       (unreachables wiki)
    assertEqCfg ctxt ctxt "reachability" expected $ removeUnreachables wiki

    where
    ctxt :: V String String -> Doc
    ctxt v = text $ case v of
			NT nt -> nt
			T t -> t
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
