-- | Sets of lookahead symbols.
module Data.Cfg.LookaheadSet (
    LookaheadSet,
    toSet
    ) where

import Data.Cfg.Augment(AugT(..))
import Data.Monoid(Monoid(..))
import qualified Data.Set as S

-- | Set of lookahead symbols providing different 'Monoid' semantics
-- than 'Data.Set.Set'.	 ('mappend' implements concatenation, not set
-- union.)
newtype LookaheadSet t = LookaheadSet {
    toSet :: S.Set (AugT t)
	-- ^ Converts the 'LookaheadSet' to a regular 'Data.Set.Set'
    }
    deriving (Eq, Ord, Show)

instance Ord t => Monoid (LookaheadSet t) where
    mempty = LookaheadSet $ S.singleton EOF
    mappend l@(LookaheadSet s) (LookaheadSet s')
	= if EOF `S.member` s
	      then LookaheadSet $ S.delete EOF s `S.union` s'
              else l

