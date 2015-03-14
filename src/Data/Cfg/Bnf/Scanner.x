{
{-# OPTIONS_GHC -w #-}
-- | Scanner for Bnf
module Data.Cfg.Bnf.Scanner(scan) where

import Data.Cfg.Bnf.Token
}

%wrapper "basic"

$lower = [a-z]
$upper = [A-Z]
$lowerTail = [a-z_0-9]
$upperTail = [A-Z_0-9]

tokens :-
       $white+          ;
       \-\-.*           ;
       \:\:\=           { Token YIELDS }
       \|               { Token OR }
       \.               { Token FULL_STOP }
       $lower $lowerTail*               { Token LOWER_IDENTIFIER }
       $upper $upperTail*               { Token UPPER_IDENTIFIER }
       .                { Token ERROR }

{
-- | Tokenizes a source string.
scan :: String -> [Token]
scan = alexScanTokens
}
