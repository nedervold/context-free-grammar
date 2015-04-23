-- | Context-free grammars.
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Cfg(
    -- * Class
    Cfg(..),
    -- * Pretty-printing
    cprettyVs,
    cprettyProduction,
    cprettyCfg,
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
    Production,
    productions,
    -- * Utility functions
    eqCfg {- ,
    compareCfg -}) where

import Control.Monad(liftM4)
import Control.Monad.Reader(MonadReader, ask)
import Data.Data(Data, Typeable)
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

-- | Pretty-prints a member of 'Cfg'.  Use this to instantiate
-- 'CPretty' for specific types.  (If we write an instance of 'CPretty'
-- for all members of 'Cfg', we get overlapping instances when we try
-- to write instances for anything else.)
cprettyCfg :: (Cfg cfg t nt, MonadReader (V t nt -> Doc) m)
	   => cfg t nt -> m Doc
cprettyCfg cfg = liftM4 vcat' ss ts nts prods
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
        ps' <- mapM cprettyProd (zip [1..] $ productions cfg)
	return (text "Productions:"
		     $$ nest 4 (vcat ps'))
	where
	cprettyProd (n, prod) = do
            prod' <- cprettyProduction prod
	    return (parens (int n) <> prod')

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

-- | Pretty-prints a list of vocabulary items.
cprettyVs :: (MonadReader (V t nt -> Doc) m) => Vs t nt -> m Doc
cprettyVs vs = do
    prettyV <- ask
    return $ hsep $ map prettyV vs

-- | Productions over vocabulary symbols
type Production t nt = (nt, Vs t nt)

-- | Maps over the terminal and nonterminal symbols in a 'Production'
bimapProduction :: (t -> t') -> (nt -> nt')
		-> Production t nt -> Production t' nt'
bimapProduction f g (nt, rhs) = (g nt, bimapVs f g rhs)

-- | Maps over the terminal and nonterminal symbols in a list of
-- 'Production's.
bimapProductions :: (t -> t') -> (nt -> nt')
		-> [Production t nt] -> [Production t' nt']
bimapProductions f g = map $ bimapProduction f g

-- | Returns the productions of the grammar.
productions :: (Cfg cfg t nt) => cfg t nt -> [Production t nt]
productions cfg = do
    nt <- S.toList $ nonterminals cfg
    vs <- S.toList $ productionRules cfg nt
    return (nt, vs)

-- | Pretty-prints a 'Production'.
cprettyProduction :: (MonadReader (V t nt -> Doc) m)
		  => Production t nt -> m Doc
cprettyProduction (hd, rhs) = do
    rhs' <- cprettyVs rhs
    prettyV <- ask
    return $ hsep [prettyV (NT hd), text "::=", rhs' <> text "."]

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
