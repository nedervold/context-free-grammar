{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Data.Cfg.EpsilonProductionsTests (
    tests
    ) where

import Data.Cfg.Analysis
import Data.Cfg.Augment
import Data.Cfg.Bnf
import Data.Cfg.Cfg
import Data.Cfg.CPretty
import Data.Cfg.EpsilonProductions
import Data.Cfg.FreeCfg(toFreeCfg)
import Data.Cfg.TestGrammars(assertEqCfg, epsProds)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)
import Text.PrettyPrint(Doc, text)

tests :: Test
tests = testGroup "Data.Cfg.EpsilonProductions" [
    epsilonProductionsTest
    ]

instance CPretty (AugFreeCfg String String) (AugV String String -> Doc) where
    cpretty = cprettyCfg

epsilonProductionsTest :: Test
epsilonProductionsTest = testCase "removal of eps-productions" $ do
    assertEqCfg ctxt ctxt
	"removed epsilon-productions correctly"
	expected actual
    assertBool "removed epsilon-productions completely"
	(isEpsilonFree $ mkAnalysis expected)

    where
    actual, expected :: AugFreeCfg String String
    actual = removeEpsilonProductions $ toFreeCfg epsProds
    expected = augmentCfg $ toFreeCfg [bnf|
	b ::= Z | a Z | Z a | a Z a .
	a ::= A .
	|]

    ctxt :: AugV String String -> Doc
    ctxt v = text $ case v of
		 NT nt -> case nt of
		     StartSymbol -> "$start"
		     AugNT s -> s
	         T t -> case t of
                     EOF -> "$EOF"
                     AugT s -> s
