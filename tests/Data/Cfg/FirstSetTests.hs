module Data.Cfg.FirstSetTests (
    tests
    ) where

import Data.Cfg.Augment
import Data.Cfg.FirstSet
import Data.Cfg.LookaheadSet
import Data.Cfg.TestGrammars
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)

tests :: Test
tests = testGroup "Data.Cfg.FirstSet" [
    g0FirstSetTest,
    microFirstSetTest
    ]

g0FirstSetTest :: Test
g0FirstSetTest = testCase "g0 first-set test" $ mapM_ f tab
    where
    f :: (String, LookaheadSet String) -> IO ()
    f (nt, expected) = assertEqual msg expected (fs $ AugNT nt)
	where
	msg = "g0: firstSet(" ++ nt ++ ")"

    tab :: [(String, LookaheadSet String)]
    tab = [("e", mkLookaheadSet False $ words "F LPAREN V"),
	   ("prefix", mkLookaheadSet True ["F"]),
	   ("tail", mkLookaheadSet True ["PLUS"])]

    fs :: AugNT String -> LookaheadSet String
    fs = firstSet g0

microFirstSetTest :: Test
microFirstSetTest = testCase "micro first-set test" $ mapM_ f tab
    where
    f :: (String, LookaheadSet String) -> IO ()
    f (nt, expected) = assertEqual msg expected (fs $ AugNT nt)
	where
	msg = "micro: firstSet(" ++ nt ++ ")"

    tab :: [(String, LookaheadSet String)]
    tab = [
	    ("program", mkLookaheadSet False $ words "BEGIN"),
	    ("statement_list", mkLookaheadSet False $ words "ID READ WRITE"),
	    ("statement", mkLookaheadSet False $ words "ID READ WRITE"),
	    ("statement_tail", mkLookaheadSet True $ words "ID READ WRITE"),
	    ("expression",
		 mkLookaheadSet False $ words "ID INT_LITERAL LPAREN"),
	    ("id_list", mkLookaheadSet False $ words "ID"),
	    ("expr_list",
		 mkLookaheadSet False $ words "ID INT_LITERAL LPAREN"),
	    ("id_tail", mkLookaheadSet True $ words "COMMA"),
	    ("expr_tail", mkLookaheadSet True $ words "COMMA"),
	    ("primary", mkLookaheadSet False $ words "ID INT_LITERAL LPAREN"),
	    ("primary_tail", mkLookaheadSet True $ words "PLUS MINUS"),
	    ("add_op", mkLookaheadSet False $ words "PLUS MINUS") ]

    fs :: AugNT String -> LookaheadSet String
    fs = firstSet micro

