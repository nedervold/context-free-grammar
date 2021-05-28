-- | Sets of lookahead symbols.
module Data.Cfg.LookaheadSet
  ( LookaheadSet
  , mkLookaheadSet
  , fromList
  , toSet
    -- * Set operations
  , empty
  , singleton
  , unions
  ) where

import Data.Cfg.Augment (AugT(..))
import qualified Data.Set as S

-- | Set of lookahead symbols providing different 'Monoid' semantics
-- than 'Data.Set.Set'.  ('mappend' implements concatenation, not set
-- union.)
newtype LookaheadSet t = LookaheadSet
  { toSet :: S.Set (AugT t)
    -- ^ Converts the 'LookaheadSet' to a regular 'Data.Set.Set'
  } deriving (Eq, Ord, Show)

instance Ord t => Semigroup (LookaheadSet t) where
  l@(LookaheadSet s) <> LookaheadSet s' =
    if EOF `S.member` s
      then LookaheadSet $ S.delete EOF s `S.union` s'
      else l

instance Ord t => Monoid (LookaheadSet t) where
  mempty = LookaheadSet $ S.singleton EOF
  mappend = (<>)

-- | Creates a 'LookaheadSet'
mkLookaheadSet ::
     (Ord t)
  => Bool -- ^ true iff it has 'EOF'
  -> [t] -- ^ terminal symbols
  -> LookaheadSet t
mkLookaheadSet hasEOF = LookaheadSet . S.fromList . f . map AugT
  where
    f =
      if hasEOF
        then (EOF :)
        else id

-- | Creates a 'LookaheadSet' from a list of augmented terminals.
fromList :: Ord t => [AugT t] -> LookaheadSet t
fromList = LookaheadSet . S.fromList

-- | The empty lookahead set.
empty :: LookaheadSet t
empty = LookaheadSet S.empty

-- | Creates a singleton lookahead set.
singleton :: AugT t -> LookaheadSet t
singleton = LookaheadSet . S.singleton

-- | Returns the union of all the lookahead sets.
unions :: Ord t => [LookaheadSet t] -> LookaheadSet t
unions = LookaheadSet . S.unions . map toSet
