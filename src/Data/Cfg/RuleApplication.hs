-- | Results of application of the production rules of a grammar.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.RuleApplication(
    language,
    yields,
    directlyYields
    ) where

import Control.Monad(liftM, msum)
import Control.Monad.Omega
import Data.Cfg.Cfg
import qualified Data.DList as DL
import qualified Data.Map.Strict as M
import qualified Data.Set as S

-- | Given a grammar and a string of symbols, returns the strings
-- yielded by application of a production rule; that is, by expanding
-- one nonterminal in the string.
directlyYields :: (Cfg cfg t nt) => cfg t nt -> Vs t nt -> [Vs t nt]
directlyYields cfg vs = do
    i <- [0..length vs - 1]
    let (pre, NT nt : post) = splitAt i vs
    expansion <- S.toList $ productionRules cfg nt
    return (pre ++ expansion ++ post)

-- | Given a grammar, returns all strings yielded by application of
-- production rules.
yields :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
    => cfg t nt -> [Vs t nt]
yields cfg = map DL.toList $ runOmega $ yieldNT (startSymbol cfg)
    where
    yieldNT :: nt -> Omega (DL.DList (V t nt))
    yieldNT nt = memoMap M.! nt
      where
        memoMap :: M.Map nt (Omega (DL.DList (V t nt)))
        memoMap = M.fromList
                  [(nt', yieldNT' nt')
                  | nt' <- S.toList $ nonterminals cfg]

        yieldNT' :: nt -> Omega (DL.DList (V t nt))
        yieldNT' nt' = msum (return (DL.singleton (NT nt'))
                       : map yieldVs rhss)
            where
            rhss = S.toList $ productionRules cfg nt'


        yieldV :: V t nt -> Omega (DL.DList (V t nt))
        yieldV v = case v of
               NT nt' -> yieldNT nt'
               t -> return $ DL.singleton t

        yieldVs :: Vs t nt -> Omega (DL.DList (V t nt))
        yieldVs = liftM DL.concat . mapM yieldV


-- NOTE: you shouldn't get symbol strings repeating if the grammar is
-- unambiguous.

-- | Given a grammar, returns all strings of terminals yielded by
-- application of the production rules to the start symbol.  This is
-- the /language/ of the grammar.
language :: (Cfg cfg t nt, Ord nt) => cfg t nt -> [Vs t nt]
    -- TODO There's certainly a more efficient way to do this.
language = filter (all isT) . yields


