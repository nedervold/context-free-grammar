{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.CPretty
import Data.Cfg.Cfg
import Data.Cfg.EpsilonProductions(removeEpsilonProductions)
import Data.Cfg.FreeCfg(FreeCfg, bimapCfg, toFreeCfg)
import Data.Cfg.FreeCfgInstances()
import Data.Cfg.LeftRecursion(LR(..), isLeftRecursive, removeLeftRecursion)
import Data.Cfg.Productive(removeUnproductives)
import Data.Cfg.Reachable(removeUnreachables)
import Data.Cfg.TestGrammars(assertEqCfg, commaList, g0, leftRec)
import Data.List(isSuffixOf)
import Data.Maybe(fromJust, isJust)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.HUnit(assertBool)
import Test.QuickCheck((==>), Property)
import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    -- removeLeftRecursionProp,
    -- indirectLeftRecursionRemovalTest,
    leftRecursionDetectionTest,
    directLeftRecursionRemovalTest
    ]

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
	$ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
	$ (not . isLeftRecursive) g0

instance CPretty (FreeCfg String (LR String)) (V String (LR String) -> Doc)
    where
    cpretty = cprettyCfg

directLeftRecursionRemovalTest :: Test
directLeftRecursionRemovalTest = testCase "direct left-recursion removal" $ do
    assertBool "removed commaList's left recursion"
	       $ (not . isLeftRecursive) commaList'
    assertEqCfg ctxt ctxt "expected result" expected commaList'

    where
    commaList' :: FreeCfg String (LR String)
    commaList' = removeLeftRecursion commaList

    ctxt :: V String (LR String) -> Doc
    ctxt v = text $ case v of
		 T t -> t
		 NT (LR nt) -> nt
		 NT (LRTail nt) -> nt ++ "_tail"

    expected :: FreeCfg String (LR String)
    expected = bimapCfg id f [bnf|
	comma_list ::= ELMT comma_list_tail.
	comma_list_tail ::= .
	comma_list_tail ::= COMMA ELMT comma_list_tail .
	|]
	where
	f :: String -> LR String
	f str = if "_tail" `isSuffixOf` str
		    then LRTail
			     $ (reverse . drop (length "_tail") . reverse) str
		    else LR str

removeLeftRecursionProp :: Test
removeLeftRecursionProp = testProperty "removeLeftRecursion does" f
    where
    f :: FreeCfg Int Int -> Property
    f cfg = isJust mCfg' ==> (not . isLeftRecursive)
				 $ removeLeftRecursion
				     $ removeEpsilonProductions
					 $ fromJust mCfg'
	where
	mCfg' = removeUnproductives $ removeUnreachables cfg

indirectLeftRecursionRemovalTest :: Test
indirectLeftRecursionRemovalTest
    = testCase "indirect left-recursion removal" $ do
	  print $ cpretty cfg (\ v -> text (case v of NT nt -> nt; T t -> t))
	  assertBool "indirectLeftRec grammar's recursion is detected"
	      $ isLeftRecursive cfg
	  print $ cprettyCfg cfg' ctxt
	  error "boom!"

    where
    cfg :: FreeCfg String String
    cfg = toFreeCfg [bnf|
	       a ::= b B | c .
	       b ::= a A | d .
               c ::= C.
               d ::= D. |]

    cfg' :: FreeCfg String (LR String)
    cfg' = removeLeftRecursion cfg

    ctxt :: V String (LR String) -> Doc
    ctxt v = text $ case v of
                 T t -> t
                 NT (LR nt) -> nt
                 NT (LRTail nt) -> nt ++ "_tail"
