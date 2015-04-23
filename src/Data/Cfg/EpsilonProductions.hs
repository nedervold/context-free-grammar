-- | Functionality for detecting and removing epsilon-productions.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.EpsilonProductions (
    EP(..),
    isEpsilonFree,
    removeEpsilonProductions
    ) where

import Control.Monad
import Data.Cfg.Cfg
import Data.Cfg.FreeCfg
import Data.Cfg.Nullable
import qualified Data.Set as S

-- | Nonterminal wrapper to introduce a symbol for the non-nullable
-- start nonterminal when removing epsilons.
data EP nt = EP nt	-- ^ non-nullable versions of the symbol
    | EPStart nt
	-- ^ nullable version of the symbol (only the start symbol can
	-- be nullable)
    deriving (Eq, Ord, Show)

-- | A slight misnomer: returns true if the analysis's grammar is
-- epsilon-free /except/ possibly at the start symbol.
isEpsilonFree :: (Cfg cfg t nt, Ord nt) => cfg t nt -> Bool
isEpsilonFree cfg = S.null ns
    where
    ns = startSymbol cfg `S.delete` nullables cfg

-- | Given a grammar, returns its augmented grammar with
-- epsilon-productions removed.
removeEpsilonProductions :: forall t nt
			 . (Ord nt, Ord t)
			 => FreeCfg t nt
			 -> FreeCfg t (EP nt)
removeEpsilonProductions cfg = FreeCfg {
    nonterminals' = if startIsNullable
	then EPStart oldStart `S.insert` S.map EP (nonterminals cfg)
	else S.map EP (nonterminals cfg),
    terminals' = terminals cfg,
    productionRules' = rules',
    startSymbol' = if startIsNullable then EPStart oldStart else EP oldStart
    }
    where
    ns :: S.Set nt
    ns = nullables cfg

    oldStart :: nt
    oldStart = startSymbol cfg

    startIsNullable :: Bool
    startIsNullable = oldStart `S.member` ns

    rules' (EPStart nt) = S.fromList [[NT (EP nt)], []]
    rules' (EP nt) = nonnullableRhss nt

    nonnullableRhss :: nt -> S.Set (Vs t (EP nt))
    nonnullableRhss nt = S.fromList
			     $ filter (not . null)
				 $ concatMap expandRhs
				     $ S.toList
					 $ productionRules cfg nt
	where
	expandRhs :: Vs t nt -> [Vs t (EP nt)]
	expandRhs [] = return []
	expandRhs (v : vs) = case v of
	    T t -> liftM (T t:) rest
	    NT nt' -> liftM (NT (EP nt'):) rest
                          `mplus` (if nt' `S.member` ns
                                       then rest
                                       else mzero)
            where
            rest :: [Vs t (EP nt)]
            rest = expandRhs vs
