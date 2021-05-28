-- | Sample grammars for tests
{-# LANGUAGE QuasiQuotes #-}

module Data.Cfg.TestGrammars
    -- * Assertion for equality in 'Cfg'
  ( assertEqCfg
    -- * Grammars for sanity checks
  , g0
  , micro
  , wiki
    -- * Analysis of grammars for sanity checks
  , g0Analysis
  , microAnalysis
  , wikiAnalysis
    -- * Convenience functions for the REPL
  , pretty'
  ) where

import Data.Cfg.Analysis
import Data.Cfg.Augment
import Data.Cfg.Bnf
import Data.Cfg.CPretty
import Data.Cfg.Cfg (Cfg(..), V(..), eqCfg)

import Test.HUnit (assertBool)
-- import Data.Cfg.FreeCfg
import Text.PrettyPrint

-- | An assertion for testing equality of 'Cfg'.
assertEqCfg ::
     ( Cfg cfg t nt
     , CPretty (cfg t nt) ctxt
     , Cfg cfg' t nt
     , CPretty (cfg' t nt) ctxt'
     , Eq t
     , Eq nt
     )
  => ctxt
  -> ctxt'
  -> String
  -> cfg t nt
  -> cfg' t nt
  -> IO ()
assertEqCfg ctxt ctxt' msg expected actual =
  assertBool msg' (eqCfg expected actual)
  where
    msg' = show $ vcat [text msg, expected', actual']
    expected' = text "Expected:" <+> cpretty expected ctxt
    actual' = text "Actual:" <+> cpretty actual ctxt'

pretty' :: AugFreeCfg String String -> Doc
pretty' cfg = cpretty cfg ctxt
  where
    ctxt :: AugV String String -> Doc
    ctxt v =
      text $
      case v of
        NT nt ->
          case nt of
            StartSymbol -> "$start"
            AugNT s -> s
        T t ->
          case t of
            EOF -> "$EOF"
            AugT s -> s

-- | A test grammar.  Found in Crafting a compiler, by Charles
-- N. Fischer and Richard J. LeBlanc, Jr., (c) 1998, pg. 95.
g0 :: Grammar String String
g0 =
  [bnf|
    e ::= prefix LPAREN e RPAREN.
    e ::= V tail.
    prefix ::= F.
    prefix ::= .
    tail ::= PLUS e.
    tail ::= .
   |]

-- | A test grammar.  Found in Fischer and LeBlanc, pg. 111.
micro :: Grammar String String
micro =
  [bnf|
    program ::= BEGIN statement_list END.
    statement_list ::= statement statement_tail.
    statement_tail ::= statement statement_tail.
    statement_tail ::=.
    statement ::= ID ASSIGN expression SEMI.
    statement ::= READ LPAREN id_list RPAREN SEMI.
    statement ::= WRITE LPAREN expr_list RPAREN SEMI.
    id_list ::= ID id_tail.
    id_tail ::= COMMA ID id_tail.
    id_tail ::=.
    expr_list ::= expression expr_tail.
    expr_tail ::= COMMA expression expr_tail.
    expr_tail ::=.
    expression ::= primary primary_tail.
    primary_tail ::= add_op primary primary_tail.
    primary_tail ::=.
    primary ::= LPAREN expression RPAREN.
    primary ::= ID.
    primary ::= INT_LITERAL.
    add_op ::= PLUS.
    add_op ::= MINUS.
    |]

-- | A test grammar.  Found at
-- http://en.wikipedia.org/wiki/Useless_rules; retrieved 2015-03-14.
wiki :: Grammar String String
wiki =
  [bnf|
    s ::= b B | c C | e E.
    b ::= b B | B.
    c ::= c C | C.
    d ::= b D | c D | D.
    e ::= e E.
    |]

g0Analysis :: Analysis String String
g0Analysis = mkAnalysis g0

microAnalysis :: Analysis String String
microAnalysis = mkAnalysis micro

wikiAnalysis :: Analysis String String
wikiAnalysis = mkAnalysis wiki
