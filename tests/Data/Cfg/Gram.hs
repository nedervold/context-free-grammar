{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -w #-}
module Data.Cfg.Gram(gram, gramToCfg') where

import Data.Cfg.Cfg hiding(V(..))
import qualified Data.Cfg.Cfg as Cfg
import qualified Data.Set as S
import Language.Haskell.TH.Quote
import Language.LBNF

bnfc [lbnf|

    terminator nonempty Prod "" ;
    separator nonempty Alt "|" ;
    terminator V "" ;

    Gram. Gram ::= [Prod] ;
    Prod. Prod ::= LOWER "::=" [Alt] "." ;
    Alt. Alt ::= [V] ;
    NT. V ::= LOWER ;
    T. V ::= UPPER ;

    token LOWER (lower (lower | digit | '_')* ) ;
    token UPPER (upper (upper | digit | '_')* ) ;

    |]

gramToCfg' :: Gram -> Cfg' String String
gramToCfg' (Gram ps) = Cfg' {
    nonterminals' = S.fromList [ nt | NT (LOWER nt) <- vocab ],
    terminals' = S.fromList [ t | T (UPPER t) <- vocab ],
    productionRules' = prodRules,
    startSymbol' = ss
    }
    where
    Prod (LOWER ss) _ : _ = ps

    prodRules :: String -> S.Set (Cfg.Vs String String)
    prodRules nt
	= S.fromList
	      $ concat [ map convertAlt alts | Prod (LOWER nt') alts <- ps,
					       nt == nt' ]
	where
	convertAlt :: Alt -> [Cfg.V String String]
	convertAlt (Alt vs) = map convertV vs

	convertV :: V -> Cfg.V String String
	convertV (NT (LOWER s)) = Cfg.NT s
	convertV (T (UPPER s)) = Cfg.T s

    vocab :: [V]
    vocab = concatMap prodVocab ps
	where
	prodVocab :: Prod -> [V]
	prodVocab (Prod hd as) = NT hd : concatMap altVocab as

        altVocab :: Alt -> [V]
        altVocab (Alt vs) = vs