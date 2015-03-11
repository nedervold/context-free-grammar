{-# LANGUAGE QuasiQuotes #-}
module Data.Cfg.TestGrammars (
    g0, micro
    ) where

import Data.Cfg.Augment
import Data.Cfg.FreeCfg
import Data.Cfg.Gram

g0 :: FreeCfg (AugT String) (AugNT String)
g0 = augmentCfg $ gramToFreeCfg [gram|
    e ::= prefix LPAREN e RPAREN.
    e ::= V tail.
    prefix ::= F.
    prefix ::= .
    tail ::= PLUS e.
    tail ::= .
   |]

micro :: FreeCfg (AugT String) (AugNT String)
micro = augmentCfg $ gramToFreeCfg [gram|
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

