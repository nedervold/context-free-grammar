{
-- | Parser for Bnf
module Data.Cfg.Bnf.Parser(parse) where

import qualified Data.Map as M
import Data.Cfg.Bnf.Scanner(scan)
import Data.Cfg.Bnf.Syntax
import Data.Cfg.Bnf.Token
import Data.Cfg(Production, V(..), Vs)
}

%name parseTokens grammar

%error { parseError }

%tokentype { Token }

%token
    FULL_STOP           { Token FULL_STOP $$ }
    LOWER_IDENTIFIER    { Token LOWER_IDENTIFIER $$ }
    OR                  { Token OR $$ }
    UPPER_IDENTIFIER    { Token UPPER_IDENTIFIER $$ }
    YIELDS              { Token YIELDS $$ }

%%

grammar :: { Grammar String String  }
        : prods { Grammar $1 }

prods :: { [Production String String] }
      : prods prod { $1 ++ $2 }
      | prod { $1 }

prod :: { [Production String String] }
     : LOWER_IDENTIFIER YIELDS rhs FULL_STOP { [ ($1, alt) | alt <- $3 ] }

rhs :: { [Vs String String] }
    : alts { $1 }

alts :: { [Vs String String] }
     : alts OR alt { $1 ++ [ $3 ] }
     | alt { [ $1 ] }

alt :: { Vs String String }
    : terms { $1 }

terms :: { Vs String String }
      : terms term { $1 ++ [ $2 ] }
      | { [] }

term :: { V String String }
     : UPPER_IDENTIFIER { T $1 }
     | LOWER_IDENTIFIER { NT $1 }

{

parseError :: [Token] -> a
parseError ts = error $ "parseError at: " ++ show ts

-- | Parses Bnf source into a 'Grammar'.
parse :: String -> Grammar String String
parse = parseTokens . scan

-- | Parses a list of 'Token's into a 'Grammar'.
parseTokens :: [Token] -> Grammar String String
}
