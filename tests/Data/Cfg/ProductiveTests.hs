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
import Data.Maybe(fromJust, isJust)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertEqual)
import Test.QuickCheck((==>), Property)
import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.Productive" [
    wikiTest,
    removeUnproductivesProp
    ]

wikiTest :: Test
wikiTest = testCase "wiki productivity test" $ do
    assertEqual "productives" (S.fromList $ grammarProductions prods')
			      (productives wiki)
    assertEqual "unproductives" (S.fromList $ grammarProductions unprods')
				(unproductives wiki)
    assertEqCfg ctxt ctxt "productivity"
		expected (fromJust $ removeUnproductives wiki)

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

removeUnproductivesProp :: Test
removeUnproductivesProp = testProperty "removeUnproductives does" f
    where
    f :: FreeCfg Int Int -> Property
    f cfg = isJust mCfg ==> S.null $ unproductives $ fromJust mCfg
        where
        mCfg = removeUnproductives cfg
