-- | Results of application of the production rules of a grammar.
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.RuleApplication(
    language,
    yields,
    directlyYields
    ) where

import Control.Applicative((<|>))
import Control.Monad(msum)
import qualified Control.Monad.WeightedSearch as W
import Data.Cfg.Cfg
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


