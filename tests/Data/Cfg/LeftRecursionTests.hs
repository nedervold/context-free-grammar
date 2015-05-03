module Data.Cfg.LeftRecursionTests (
    tests
    ) where

import Data.Cfg.Instances()
import Data.Cfg.LeftRecursion(isLeftRecursive)
import Data.Cfg.TestGrammars(g0, leftRec)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertBool)

tests :: Test
tests = testGroup "Data.Cfg.LeftRecursion" [
    leftRecursionDetectionTest
    ]

leftRecursionDetectionTest :: Test
leftRecursionDetectionTest = testCase "left-recursion detection" $ do
    assertBool "leftRec grammar's recursion is detected"
	$ isLeftRecursive leftRec
    assertBool "g0 grammar's lack of recursion is detected"
	$ (not . isLeftRecursive) g0

{-
directLeftRecursionRemovalTest :: Test
directLeftRecursionRemovalTest = testCase "direct left-recursion removal" $ do
    assertBool "removed commaList's left recursion"
	       $ (not . isLeftRecursive) commaList'
    assertEqCfg' "expected result" expected commaList'

    where
    commaList' :: FreeCfg String (LR String)
    commaList' = removeLeftRecursion commaList

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
removeLeftRecursionProp = testProperty "removeLeftRecursion does" $
    (not . isLeftRecursive)
	. removeLeftRecursion
	    . removeEpsilonProductions
		. unProductiveEpsFreeCfg
-}

{-
    cfg :: FreeCfg String String
    cfg = toFreeCfg [bnf|
               a ::= b B | c .
               b ::= a A | d .
               c ::= C.
               d ::= D. |]
-}


