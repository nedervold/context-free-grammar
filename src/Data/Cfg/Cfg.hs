-- | Context-free grammars.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Cfg(
    -- * Class
    Cfg(..),
    -- * Vocabulary
    V(..),
    isNT,
    isT,
    Vs,
    vocabulary,
    usedVocabulary,
    undeclaredVocabulary,
    isFullyDeclared,
    -- * Productions
    Production,
    productions {- ,
    -- * Utility functions
    eqCfg,
    compareCfg -}) where

import Control.Monad(liftM4)
import Control.Monad.Reader(ask)
import Data.Bifunctor(Bifunctor(..))
import Data.Cfg.CPretty
import qualified Data.Set as S
import Text.PrettyPrint

------------------------------------------------------------

-- | Represents a context-free grammar with its nonterminal and
-- terminal types.
class Cfg cfg t nt where
    nonterminals :: cfg t nt -> S.Set nt
	-- ^ the nonterminals of the grammar
    terminals :: cfg t nt -> S.Set t
	-- ^ the terminals of the grammar
    productionRules :: cfg t nt -> nt -> S.Set (Vs t nt)
	-- ^ the productions of the grammar
    startSymbol :: cfg t nt -> nt
	-- ^ the start symbol of the grammar; must be an element of
	-- 'nonterminals' 'cfg'

instance (Cfg cfg t nt) => CPretty (cfg t nt) (V t nt -> Doc) where
    cpretty cfg = liftM4 vcat' ss ts nts prods
	where
	vcat' a b c d = vcat [a, b, c, d]
	ss = do
	    prettyV <- ask
	    return (text "Start symbol:" <+> prettyV (NT $ startSymbol cfg))
	ts = do
	    prettyV <- ask
	    return (text "Terminals:"
		       <+> fsep (punctuate comma
				   $ map (prettyV . T)
					 (S.toList $ terminals cfg)))
	nts = do
	    prettyV <- ask
	    return (text "Nonterminals:"
		       <+> fsep (punctuate comma
				   $ map (prettyV . NT)
					 (S.toList $ nonterminals cfg)))

	prods = do
	    prettyV <- ask
	    return (text "Productions:"
			 $$ nest 4
				 (vcat (map (prettyProd prettyV)
					    (zip [1..] $ productions cfg))))
	    where
	    prettyProd pv (n, (hd, rhs))
		= hsep [parens (int n),
			pv (NT hd), text "::=", rhs' <> text "."]
		where
		rhs' = hsep $ map pv rhs

------------------------------------------------------------

------------------------------------------------------------

-- | Vocabulary symbols of the grammar.
data V t nt = T t	-- ^ a terminal
    | NT nt		-- ^ a nonterminal
    deriving (Eq, Ord, Show)

-- | Returns 'True' iff the vocabularly symbols is a terminal.
isT :: V t nt -> Bool
isT (T _) = True
isT _ = False

-- | Returns 'True' iff the vocabularly symbols is a nonterminal.
isNT :: V t nt -> Bool
isNT (NT _) = True
isNT _ = False

instance Functor (V t) where
    fmap _f (T t) = T t
    fmap f (NT nt) = NT $ f nt

instance Bifunctor V where
    bimap f _g (T t) = T $ f t
    bimap _f g (NT nt) = NT $ g nt

-- | Returns the vocabulary symbols of the grammar: elements of
-- 'terminals' and 'nonterminals'.
vocabulary :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> S.Set (V t nt)
vocabulary cfg = S.map T (terminals cfg)
		     `S.union` S.map NT (nonterminals cfg)

-- | Synonym for lists of vocabulary symbols.
type Vs t nt = [V t nt]

-- | Productions over vocabulary symbols
type Production t nt = (nt, Vs t nt)

-- | Returns the productions of the grammar.
productions :: (Cfg cfg t nt) => cfg t nt -> [Production t nt]
productions cfg = do
    nt <- S.toList $ nonterminals cfg
    vs <- S.toList $ productionRules cfg nt
    return (nt, vs)

{------------------------------------------------------------

-- | Returns 'True' iff the two inhabitants of 'Cfg' are equal.
eqCfg :: forall cfg cfg' t nt
      . (Cfg cfg t nt, Cfg cfg' t nt, Eq nt, Eq t)
      => cfg t nt -> cfg' t nt -> Bool
eqCfg cfg cfg' = to4Tuple cfg == to4Tuple cfg'

-- | Compares the two inhabitants of 'Cfg'.
compareCfg :: forall cfg cfg' t nt
	   . (Cfg cfg t nt, Cfg cfg' t nt, Ord nt, Ord t)
	   => cfg t nt -> cfg' t nt -> Ordering
compareCfg cfg cfg' = compare (to4Tuple cfg) (to4Tuple cfg')

-- | Converts the 'Cfg' to a 4-tuple that inhabits both 'Eq' and 'Ord'
-- if 't' and 'nt' do.
to4Tuple :: forall cfg t nt . (Cfg cfg t nt)
	 => cfg t nt -> (nt, S.Set nt, S.Set t, [Production t nt])

    -- We move the start symbol first to optimize the operations
    -- since it's most likely to differ.

to4Tuple cfg = (
    startSymbol cfg,
    nonterminals cfg,
    terminals cfg,
    productions cfg)

------------------------------------------------------------}

-- | Returns all vocabulary used in the productions plus the start
-- symbol.
usedVocabulary :: (Cfg cfg t nt, Ord nt, Ord t)
	       => cfg t nt -> S.Set (V t nt)
usedVocabulary cfg
    = S.fromList
	  $ NT (startSymbol cfg) :
	      concat [ NT nt : vs | (nt, vs) <- productions cfg]

-- | Returns all vocabulary used in the productions plus the start
-- symbol but not declared in 'nonterminals' or 'terminals'.
undeclaredVocabulary :: (Cfg cfg t nt, Ord nt, Ord t)
                     => cfg t nt -> S.Set (V t nt)
undeclaredVocabulary cfg = usedVocabulary cfg S.\\ vocabulary cfg

------------------------------------------------------------

-- | Returns 'True' all the vocabulary used in the grammar is
-- declared.
isFullyDeclared :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> Bool
isFullyDeclared = S.null . undeclaredVocabulary
