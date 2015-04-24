-- | Context-free grammars.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Cfg(
    -- * Class
    Cfg(..),
    -- * Vocabulary
    V(..),
    Vs,
    isNT,
    isT,
    bimapV,
    bimapVs,
    bimapProduction,
    bimapProductions,
    vocabulary,
    usedVocabulary,
    undeclaredVocabulary,
    isFullyDeclared,
    -- * Productions
    Production(..),
    productions,
    lookupProductions,
    -- * Utility functions
    eqCfg {- ,
    compareCfg -}) where

import Data.Data(Data, Typeable)
import qualified Data.Set as S

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

------------------------------------------------------------

-- | Vocabulary symbols of the grammar.
data V t nt = T t	-- ^ a terminal
    | NT nt		-- ^ a nonterminal
    deriving (Eq, Ord, Show, Data, Typeable)

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

-- | Maps over the terminal and nonterminal symbols in a 'V'.
bimapV :: (t -> t') -> (nt -> nt') -> V t nt -> V t' nt'
bimapV f _g (T t) = T $ f t
bimapV _f g (NT nt) = NT $ g nt

-- | Returns the vocabulary symbols of the grammar: elements of
-- 'terminals' and 'nonterminals'.
vocabulary :: (Cfg cfg t nt, Ord nt, Ord t) => cfg t nt -> S.Set (V t nt)
vocabulary cfg = S.map T (terminals cfg)
		     `S.union` S.map NT (nonterminals cfg)

-- | Synonym for lists of vocabulary symbols.
type Vs t nt = [V t nt]

-- | Maps over the terminal and nonterminal symbols in a list of 'V's.
bimapVs :: (t -> t') -> (nt -> nt') -> Vs t nt -> Vs t' nt'
bimapVs f g = map (bimapV f g)

-- | Productions over vocabulary symbols
data Production t nt = Production {
    productionHead :: nt,
    productionRhs :: Vs t nt
    }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | Maps over the terminal and nonterminal symbols in a 'Production'
bimapProduction :: (t -> t') -> (nt -> nt')
		-> Production t nt -> Production t' nt'
bimapProduction f g (Production nt rhs) = Production (g nt) (bimapVs f g rhs)

-- | Maps over the terminal and nonterminal symbols in a list of
-- 'Production's.
bimapProductions :: (t -> t') -> (nt -> nt')
		-> [Production t nt] -> [Production t' nt']
bimapProductions f g = map $ bimapProduction f g

-- | The productions of the grammar.
productions :: (Cfg cfg t nt) => cfg t nt -> [Production t nt]
productions cfg = do
    nt <- S.toList $ nonterminals cfg
    vs <- S.toList $ productionRules cfg nt
    return $ Production nt vs

-- | The productions for the given nonterminal
lookupProductions :: Eq nt => nt -> [Production t nt] -> [Vs t nt]
lookupProductions nt prods = [ rhs | Production nt' rhs <- prods,
				     nt == nt' ]

-- | Returns 'True' iff the two inhabitants of 'Cfg' are equal.
eqCfg :: forall cfg cfg' t nt
      . (Cfg cfg t nt, Cfg cfg' t nt, Eq nt, Eq t)
      => cfg t nt -> cfg' t nt -> Bool
eqCfg cfg cfg' = to4Tuple cfg == to4Tuple cfg'

{------------------------------------------------------------

-- | Compares the two inhabitants of 'Cfg'.
compareCfg :: forall cfg cfg' t nt
	   . (Cfg cfg t nt, Cfg cfg' t nt, Ord nt, Ord t)
	   => cfg t nt -> cfg' t nt -> Ordering
compareCfg cfg cfg' = compare (to4Tuple cfg) (to4Tuple cfg')

------------------------------------------------------------}

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

-- | Returns all vocabulary used in the productions plus the start
-- symbol.
usedVocabulary :: (Cfg cfg t nt, Ord nt, Ord t)
	       => cfg t nt -> S.Set (V t nt)
usedVocabulary cfg
    = S.fromList
	  $ NT (startSymbol cfg) :
	      concat [ NT nt : vs | Production nt vs <- productions cfg]

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
