{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.GramTests(tests) where

import Data.Cfg.Cfg
import Data.Cfg.Gram
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)

tests :: Test
tests = testGroup "gram quasiquoter tests" [ test ]

test :: Test
test = testCase "gram quasiquoter sanity test" $ do
    assertEqual "startSymbol works" "foo" (startSymbol cfg')
    assertEqual "terminals works" 5 (S.size $ terminals cfg')
    assertEqual "nonterminals works" 2 (S.size $ nonterminals cfg')
    assertEqual "productions count works" 3 (length $ productions cfg')
    where
    cfg' = gramToFreeCfg gram'
    gram' = [gram|foo ::= A B C D bar.
		  foo ::= .
		  bar ::= E A B. |]

{- Test code for yieldCfg.
import Data.Cfg.RuleApplication

py :: Int -> IO ()
py n = mapM_ print $ take n y
    where
    y = yieldCfg cfg
    cfg = gramToCfg' gram'
    gram' = [gram|digits ::= digit | digit digits .
	          digit ::= O | I . |]

-}
