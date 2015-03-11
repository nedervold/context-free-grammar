-- | The iterative fixed-point function.
module Data.Cfg.FixedPoint (
    fixedPoint
    ) where

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

-- TODO When I can use fix instead?