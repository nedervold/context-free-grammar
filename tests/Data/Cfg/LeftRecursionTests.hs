{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Data.Cfg.Bnf
import Data.Cfg.Cfg
import Data.Cfg.LeftRecursion(LR(..), isLeftRecursive, removeLeftRecursion)
import Data.Cfg.TestGrammars(commaList, g0, leftRec)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)

import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    leftRecursionDetectionTest,
    directLeftRecursionRemovalTest
    ]

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
	$ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
	$ (not . isLeftRecursive) g0

directLeftRecursionRemovalTest :: Test
directLeftRecursionRemovalTest = testCase "direct left-recursion remove" $
    assertBool "removed commaList's left recursion"
	$ (not . isLeftRecursive) commaList'

    -- TODO check expected against commaList'.	But they're of
    -- different types.	 Need to do something about that.

    -- print $ cprettyCfg commaList' ctxt

    where
    commaList' = removeLeftRecursion commaList

    _ctxt :: V String (LR String) -> Doc
    _ctxt v = text $ case v of
		 T t -> t
		 NT (LR nt) -> nt
		 NT (LRTail nt) -> nt ++ "_tail"

    _expected = [bnf|
	comma_list ::= ELMT comma_list_tail.
	comma_list_tail ::= .
        comma_list_tail ::= COMMA ELMT comma_list_tail .
        |]