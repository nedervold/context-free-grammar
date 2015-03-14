-- | 'QuasiQuoter' for BNF source.
module Data.Cfg.Bnf.QQ(bnf) where

import Data.Cfg.Bnf.Parser
import Language.Haskell.TH.Quote

-- | 'QuasiQuoter' for BNF source.  Generates a value of type
-- 'Grammar'.  Not usable in pattern, type or declaration positions.
bnf :: QuasiQuoter
bnf = QuasiQuoter {
    quoteExp = dataToExpQ (const Nothing) . parse,
    quotePat = err,
    quoteType = err,
    quoteDec = err
    }
    where
    err _ = error "The bnf quasiquoter is only allowed in Exp position."