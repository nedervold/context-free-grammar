-- | Syntax structures of Bnf.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Cfg.Bnf.Syntax(Grammar(..)) where

import Data.Cfg.Cfg(Cfg(..), Production, V(..), Vs)
import Data.Data(Data, Typeable)
import qualified Data.Set as S

-- | A simple, concrete instance of 'Cfg'.  The terminal and
-- nonterminal symbols of a 'Grammar' are defined to be exactly those
-- appearing the productions.  The start symbol is defined to be the
-- head of the first of the productions.
newtype Grammar t nt = Grammar {
    grammarProductions :: [Production t nt]
    -- ^ the productions of the 'Grammar'
    }
    deriving (Data, Typeable)

instance (Ord nt, Ord t) => Cfg Grammar t nt where
    terminals = S.fromList . concatMap terminalsProd . grammarProductions
    nonterminals = S.fromList . concatMap nonterminalsProd . grammarProductions
    productionRules g nt
    = S.fromList [ rhs | (nt', rhs) <- grammarProductions g,
                 nt == nt' ]
    startSymbol = fst . head . grammarProductions

nonterminalsVs :: Vs t nt -> [nt]
nonterminalsVs vs = [ nt | NT nt <- vs ]

terminalsVs :: Vs t nt -> [t]
terminalsVs vs = [ t | T t <- vs ]

nonterminalsProd :: Production t nt -> [nt]
nonterminalsProd (nt, rhs) = nt : nonterminalsVs rhs

terminalsProd :: Production t nt -> [t]
terminalsProd = terminalsVs . snd
