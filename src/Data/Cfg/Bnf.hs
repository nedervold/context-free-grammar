{- | A simple, concrete instance of 'Cfg' that can be parsed from source.

The grammar of Bnf source is:

@
grammar ::= (production)+.
production ::= LOWER_CASE_STRING "::=" right_hand_sides ".".
right_hand_sides ::= right_hand_side ("|" right_hand_side)*.
right_hand_side ::= term*.
term ::= UPPER_CASE_STRING | LOWER_CASE_STRING.
@

where /*/ means zero or more repetitions and /+/ means one or more
repetitions.  Terminals are indicated by strings of upper-case
characters and underscores; nonterminals by strings of lower-case
characters and underscores.  Quoted strings are literal tokens.

-}

module Data.Cfg.Bnf (
    Grammar(..),
    bnf,
    parse
    ) where

import Data.Cfg.Bnf.Parser
import Data.Cfg.Bnf.QQ
import Data.Cfg.Bnf.Syntax

