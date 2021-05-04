module Data.Cfg.FollowSetTests (
    tests
    ) where

import Data.Cfg.Analysis
import Data.Cfg.Augment
import Data.Cfg.LookaheadSet
import Data.Cfg.TestGrammars
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)

tests :: Test
tests = testGroup "Data.Cfg.FollowSet" [
    g0FollowSetTest,
    microFollowSetTest
    ]

g0FollowSetTest :: Test
g0FollowSetTest = testCase "g0 follow-set test" $ mapM_ f tab
    where
    f :: (String, LookaheadSet String) -> IO ()
    f (nt, expected) = assertEqual msg expected (fols $ AugNT nt)
        where
        msg = "g0: followSet(" ++ nt ++ ")"

    tab :: [(String, LookaheadSet String)]
    tab = [("e", mkLookaheadSet True ["RPAREN"]),
           ("prefix", mkLookaheadSet False ["LPAREN"]),
           ("tail", mkLookaheadSet True ["RPAREN"])]

    fols :: AugNT String -> LookaheadSet String
    fols = followSet g0Analysis

microFollowSetTest :: Test
microFollowSetTest = testCase "micro follow-set test" $ mapM_ f tab
    where
    f :: (String, LookaheadSet String) -> IO ()
    f (nt, expected) = assertEqual msg expected (fols $ AugNT nt)
        where
        msg = "micro: followSet(" ++ nt ++ ")"

    tab :: [(String, LookaheadSet String)]
    tab = [ ("program", mkLookaheadSet True []),
            ("statement_list", mkLookaheadSet False $ words "END"),
            ("statement", mkLookaheadSet False $ words "ID READ WRITE END"),
            ("statement_tail", mkLookaheadSet False $ words "END"),
            ("expression", mkLookaheadSet False $ words "COMMA SEMI RPAREN"),
            ("id_list", mkLookaheadSet False $ words "RPAREN"),
            ("expr_list", mkLookaheadSet False $ words "RPAREN"),
            ("id_tail", mkLookaheadSet False $ words "RPAREN"),
            ("expr_tail", mkLookaheadSet False $ words "RPAREN"),
            ("primary",
                 mkLookaheadSet False $ words "COMMA SEMI PLUS MINUS RPAREN"),
            ("primary_tail", mkLookaheadSet False $ words "COMMA SEMI RPAREN"),
            ("add_op", mkLookaheadSet False $ words "ID INT_LITERAL LPAREN") ]

    fols :: AugNT String -> LookaheadSet String
    fols = followSet microAnalysis

