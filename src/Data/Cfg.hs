-- | Context-free grammars.
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg(
    -- * Class
    Cfg(..),
    -- * Canonical instance of 'Cfg'
    Cfg'(..),
    -- * Auxiliary data types
    V(..),
    Vs,
    Production,
    -- * Query
    productions,
    vocabulary,
    usedVocabulary,
    undeclaredVocabulary,
    isFullyDeclared,
    -- * Utility functions
    eqCfg,
    compareCfg) where

import Data.Bifunctor(Bifunctor(..))
import qualified Data.Set as S

------------------------------------------------------------

-- | Represents a context-free grammar with its nonterminal and
-- terminal types.
class Cfg cfg t nt where
    nonterminals :: Ord nt => cfg t nt -> S.Set nt
	-- ^ the nonterminals of the grammar
    terminals :: Ord t => cfg t nt -> S.Set t
	-- ^ the terminals of the grammar
    productionRules :: (Ord nt, Ord t)
			   => cfg t nt -> nt -> S.Set (Vs t nt)
	-- ^ the productions of the grammar
    startSymbol :: cfg t nt -> nt
	-- ^ the start symbol of the grammar; must be an element of
	-- 'nonterminals' 'cfg'

------------------------------------------------------------

-- | Represents a context-free grammar with its nonterminal and
-- terminal types.  The canonical instance of 'Cfg': a record that
-- collects up implementations of each class method.
data Cfg' t nt = Cfg' {
    nonterminals' :: S.Set nt,
	-- ^ the nonterminals of the grammar
    terminals' :: S.Set t,
	-- ^ the terminals of the grammar
    productionRules' :: nt -> S.Set (Vs t nt),
	-- ^ the productions of the grammar
    startSymbol' :: nt
	-- ^ the start symbol of the grammar; must be an element of
	-- 'nonterminals' 'cfg'
    }

instance Cfg Cfg' t nt where
    nonterminals = nonterminals'
    terminals = terminals'
    productionRules = productionRules'
    startSymbol = startSymbol'

------------------------------------------------------------

-- | Vocabulary symbols of the grammar.
data V t nt = T t	-- ^ a terminal
    | NT nt		-- ^ a nonterminal
    deriving (Eq, Ord)

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
productions :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> [Production t nt]
productions cfg = do
    nt <- S.toList $ nonterminals cfg
    vs <- S.toList $ productionRules cfg nt
    return (nt, vs)

------------------------------------------------------------

-- | Returns 'True' iff the two inhabitants of 'Cfg' are equal.
eqCfg :: forall cfg cfg' t nt
      . (Cfg cfg t nt, Cfg cfg' t nt, Ord nt, Ord t)
      => cfg t nt -> cfg' t nt -> Bool
eqCfg cfg cfg' = to4Tuple cfg == to4Tuple cfg'

-- | Compares the two inhabitants of 'Cfg'.
compareCfg :: forall cfg cfg' t nt
	   . (Cfg cfg t nt, Cfg cfg' t nt, Ord nt, Ord t)
	   => cfg t nt -> cfg' t nt -> Ordering
compareCfg cfg cfg' = compare (to4Tuple cfg) (to4Tuple cfg')

-- | Converts the 'Cfg' to a 4-tuple that inhabits both 'Eq' and 'Ord'
-- if 't' and 'nt' do.
to4Tuple :: forall cfg t nt . (Cfg cfg t nt, Ord nt, Ord t)
	 => cfg t nt -> (nt, S.Set nt, S.Set t, [Production t nt])

    -- We move the start symbol first to optimize the operations
    -- since it's most likely to differ.

to4Tuple cfg = (
    startSymbol cfg,
    nonterminals cfg,
    terminals cfg,
    productions cfg)

------------------------------------------------------------

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
