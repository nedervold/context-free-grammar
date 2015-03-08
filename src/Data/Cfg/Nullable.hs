-- | Nullable nonterminals
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.Nullable(nullables) where

import Data.Cfg.Cfg
import Control.Monad(guard)
import qualified Data.Set as S

-- | Returns the nonterminals in the grammar that can produce the
-- empty string.
nullables :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
	  => cfg t nt -> S.Set nt
nullables cfg = fixedPoint go S.empty
    where
    go :: S.Set nt -> S.Set nt
    go knownNullables = calculatedNullables
	where
	isKnownNullable :: V t nt -> Bool
	isKnownNullable (NT nm) = nm `S.member` knownNullables
	isKnownNullable _ = False

	calculatedNullables :: S.Set nt
	calculatedNullables = S.fromList $ do
	    nt <- S.toList $ nonterminals cfg
	    let rhss = S.toList $ productionRules cfg nt
	    guard $ any (all isKnownNullable) rhss
	    return nt

-- | Given a function and an initial value, find the fixed point of
-- the function.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f = go
    where
    go s = if s == s'
	       then s
	       else go s'
	where
	s' = f s
