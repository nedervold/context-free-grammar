{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.ProductiveTests (
    tests
    ) where

import Data.Cfg.Bnf(Grammar(..), bnf)
import Data.Cfg.Cfg(Cfg(..), V(..))
import Data.Cfg.FreeCfg(FreeCfg(..), toFreeCfg)
import Data.Cfg.FreeCfgInstances()
import Data.Cfg.Productive
import Data.Cfg.TestGrammars(assertEqCfg, wiki)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)
import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.Productive" [
    wikiTest
    ]

wikiTest :: Test
wikiTest = testCase "wiki productivity test" $ do
    assertEqual "productives" (S.fromList $ grammarProductions prods')
			      (productives wiki)
    assertEqual "unproductives" (S.fromList $ grammarProductions unprods')
				(unproductives wiki)
    assertEqCfg ctxt ctxt "productivity" expected $ removeUnproductives wiki

    where
    ctxt :: V String String -> Doc
    ctxt v = text $ case v of
			NT nt -> nt
			T t -> t

    expected :: FreeCfg String String
    expected = (toFreeCfg prods') {
	terminals' = terminals wiki
	}

    prods' :: Grammar String String
    prods' = [bnf|
	s ::= b B | c C.
	b ::= b B | B.
	c ::= c C | C.
	d ::= b D | c D | D.
        |]

    unprods' :: Grammar String String
    unprods' = [bnf|
        s ::= e E.
        e ::= e E.
        |]
