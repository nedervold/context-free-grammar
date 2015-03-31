-- | The iterative fixed-point function.
module Data.Cfg.FixedPoint(fixedPoint) where

-- | Given a function and an initial value, find the fixed point of
-- the function.
fixedPoint :: Eq a => (a -> a) -> a -> a
fixedPoint f a = go $ iterate f a
    where
    go as@(a' : a'' : _) = if a' == a''
			       then a'
			       else go $ tail as
    go _ = error "fixedPoint: impossible"