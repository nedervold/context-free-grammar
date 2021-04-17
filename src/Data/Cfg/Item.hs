-- | Parsing items.
module Data.Cfg.Item (
    AugItem,
    Item,
    -- * Creation
    mkInitialItem,
    nextItem,
    -- * Query
    production,
    mark,
    atEnd,
    beforeMark,
    afterMark,
    nextV
    ) where

import Data.Cfg.Augment
import Data.Cfg.Cfg

-- | A parsing \"item\": a partially processed production.
data Item t nt = Item {
    mark :: Int,
        -- ^ The index showing how many vocabulary term of the production
        -- have been processed
    production :: Production t nt
        -- ^ The production for the 'Item'
    }
    deriving (Eq, Ord)

-- | An augmented 'Item'.
type AugItem t nt = Item (AugT t) (AugNT nt)

-- | An 'Item' representing the unprocessed production.
mkInitialItem :: Production t nt -> Item t nt
mkInitialItem = Item 0

-- | The 'Item' with one more vocabulary term processed.  If the
-- production is already completely processed, you get 'Nothing'.
nextItem :: Item t nt -> Maybe (Item t nt)
nextItem item = if atEnd item
    then Nothing
    else Just item { mark = 1 + mark item }

-- | An 'Item' representing a completely processed production.
atEnd :: Item t nt -> Bool
atEnd = null . afterMark

-- | The processed vocabulary terms of the production.
beforeMark :: Item t nt -> [V t nt]
beforeMark item  = take (mark item) $ productionRhs (production item)

-- | The unprocessed vocabulary terms of the production.
afterMark :: Item t nt -> [V t nt]
afterMark item = drop (mark item) $ productionRhs (production item)

-- | The next vocabulary term to be processed, if there is one.
nextV :: Item t nt -> Maybe (V t nt)
nextV item = case afterMark item of
                 [] -> Nothing
                 (v : _) -> Just v
