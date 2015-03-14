-- | Sample grammars for tests
{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.TestGrammars (
    -- * Grammars for testing
    g0,
    micro,
    -- * Convenience functions for the REPL
    pretty'
    ) where

import Data.Cfg.Augment
import Data.Cfg.Bnf
import Data.Cfg.Cfg(V(..))
import Data.Cfg.CPretty
import Data.Cfg.FreeCfg
import Text.PrettyPrint

pretty' :: AugFreeCfg String String -> Doc
pretty' cfg = cpretty cfg ctxt
    where
    ctxt :: AugV String String -> Doc
    ctxt v = text $ case v of
		 NT nt -> case nt of
		     StartSymbol -> "$start"
		     AugNT s -> s
		 T t -> case t of
		     EOF -> "$EOF"
		     AugT s -> s

-- | A test grammar.  Found in Crafting a compiler, by Charles
-- N. Fischer and Richard J. LeBlanc, Jr., (c) 1998, pg. 95.
g0 :: FreeCfg (AugT String) (AugNT String)
g0 = augmentCfg [bnf|
    e ::= prefix LPAREN e RPAREN.
    e ::= V tail.
    prefix ::= F.
    prefix ::= .
    tail ::= PLUS e.
    tail ::= .
   |]

-- | A test grammar.  Found in Fischer and LeBlanc, pg. 111.
micro :: FreeCfg (AugT String) (AugNT String)
micro = augmentCfg [bnf|
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
