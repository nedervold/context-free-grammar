-- | Sample grammars for tests
{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.TestGrammars (
    -- * Assertion for equality in 'Cfg'
    assertEqCfg',
    -- * Grammars for sanity checks
    commaList,
    epsProds,
    g0,
    leftRec,
    micro,
    micro',
    wiki,
    -- * Analysis of grammars for sanity checks
    commaListAnalysis,
    g0Analysis,
    leftRecAnalysis,
    microAnalysis,
    wikiAnalysis
    ) where

import Data.Cfg.Analysis
import Data.Cfg.Bnf
import Data.Cfg.Cfg(Cfg(..), eqCfg)
import Data.Cfg.Pretty(Pretty(..))
import Text.PrettyPrint
import Test.HUnit(assertBool)

-- | An assertion for testing equality of 'Cfg'.
assertEqCfg' :: (Cfg cfg t nt, Cfg cfg' t nt,
		 Pretty (cfg t nt), Pretty (cfg' t nt),
		 Eq t, Eq nt)
		=> String -> cfg t nt -> cfg' t nt -> IO ()
assertEqCfg' msg expected actual =
    assertBool msg' (eqCfg expected actual)
    where
    msg' = show $ vcat [text msg, expected', actual']
    expected' = text "Expected:" <+> pretty expected
    actual' = text "Actual:" <+> pretty actual

-- | A test grammar.  Found in Crafting a compiler, by Charles
-- N. Fischer and Richard J. LeBlanc, Jr., (c) 1998, pg. 95.
g0 :: Grammar String String
g0 = [bnf|
    e ::= prefix LPAREN e RPAREN.
    e ::= V tail.
    prefix ::= F.
    prefix ::= .
    tail ::= PLUS e.
    tail ::= .
   |]

-- | A test grammar.  Found in Fischer and LeBlanc, pg. 111.
micro :: Grammar String String
micro = [bnf|
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

-- | A variation on micro.
micro' :: Grammar String String
micro' = [bnf|
    program ::= BEGIN statement_list END.
    statement_list ::= statement statement_list.
    statement_list ::= statement.
    statement ::= ID ASSIGN expression SEMI.
    statement ::= READ LPAREN id_list RPAREN SEMI.
    statement ::= WRITE LPAREN expr_list RPAREN SEMI.
    id_list ::= ID COMMA id_list | ID.
    expr_list ::= expression COMMA expression_list.
    expr_list ::= expression.
    expression ::= primary add_op expression | primary .
    primary ::= LPAREN expression RPAREN.
    primary ::= ID.
    primary ::= INT_LITERAL.
    add_op ::= PLUS.
    add_op ::= MINUS.
    |]

-- | A test grammar.  Found at
-- http://en.wikipedia.org/wiki/Useless_rules; retrieved 2015-03-14.
wiki :: Grammar String String
wiki = [bnf|
    s ::= b B | c C | e E.
    b ::= b B | B.
    c ::= c C | C.
    d ::= b D | c D | D.
    e ::= e E.
    |]

-- | A test grammar.  Found at
-- http://web.cs.wpi.edu/~kal/PLT/PLT4.1.2.html; retrieved 2015-04-19.
leftRec :: Grammar String String
leftRec = [bnf|
    a ::= b X Y | X.
    b ::= c d .
    c ::= a | C.
    d ::= D.
    |]

-- | A test grammar.  Found at
-- http://eli.thegreenplace.net/2010/02/08/removing-epsilon-productions-from-context-free-grammars;
-- retrieved 2015-04-19.
epsProds :: Grammar String String
epsProds = [bnf|
    b ::= a Z a .
    a ::= A | .
    |]

commaList :: Grammar String String
commaList = [bnf|
    comma_list ::= comma_list COMMA ELMT
	       | ELMT .
    |]

g0Analysis :: Analysis String String
g0Analysis = mkAnalysis g0

microAnalysis :: Analysis String String
microAnalysis = mkAnalysis micro

wikiAnalysis :: Analysis String String
wikiAnalysis = mkAnalysis wiki

leftRecAnalysis :: Analysis String String
leftRecAnalysis = mkAnalysis leftRec

commaListAnalysis :: Analysis String String
commaListAnalysis = mkAnalysis commaList
