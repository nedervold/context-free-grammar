-- | Results of application of the production rules of a grammar.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.RuleApplication(
    language,
    yields,
    yieldCfg,
    directlyYields
    ) where

import Control.Applicative((<|>))
import Control.Monad(liftM, msum)
import Control.Monad.Omega
import qualified Control.Monad.WeightedSearch as W
import Data.Cfg.Cfg
import qualified Data.DList as DL
import qualified Data.Map as M
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
yieldCfg :: forall cfg t nt . (Cfg cfg t nt, Ord nt)
       => cfg t nt -> [Vs t nt]
yieldCfg cfg = map DL.toList $ runOmega $ yieldNT (startSymbol cfg)
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
		   NT nt -> yieldNT nt
		   t -> return $ DL.singleton t

    yieldVs :: Vs t nt -> Omega (DL.DList (V t nt))
    yieldVs = liftM DL.concat . mapM yieldV


-- | Given a grammar and a string of symbols, returns a computation
-- listing all strings yielded by application of the production rules.
yields' :: forall cfg t nt . (Cfg cfg t nt)
       => cfg t nt -> Vs t nt -> W.T Int (Vs t nt)
yields' cfg vs = return vs <|> msum (map weigh vss)
    where
    vss :: [Vs t nt]
    vss = directlyYields cfg vs

    weigh :: Vs t nt -> W.T Int (Vs t nt)
    weigh vs' = W.weight ntCnt (yields' cfg vs')
	where
	ntCnt :: Int
	ntCnt = length $ filter isNT vs'

-- | Given a grammar and a string of symbols, returns all strings
-- yielded by application of the production rules.
yields :: forall cfg t nt . (Cfg cfg t nt)
       => cfg t nt -> Vs t nt -> [Vs t nt]
yields cfg = W.toList . yields' cfg

-- NOTE: you shouldn't get symbol strings repeating if the grammar is
-- unambiguous.

-- | Given a grammar, returns all strings of terminals yielded by
-- application of the production rules to the start symbol.  This is
-- the /language/ of the grammar.
language :: (Cfg cfg t nt) => cfg t nt -> [Vs t nt]
language cfg = filter (all isT) $ yields cfg [NT $ startSymbol cfg]


