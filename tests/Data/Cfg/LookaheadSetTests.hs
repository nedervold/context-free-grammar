{-# LANGUAGE FlexibleInstances #-}
module Data.Cfg.LookaheadSetTests (
    tests
    ) where

import Control.Monad(liftM2)
import Data.Cfg.LookaheadSet(LookaheadSet, mkLookaheadSet)
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Test.QuickCheck(Arbitrary(..), listOf)
import Test.QuickCheck.Property.Monoid(T(..), eq, prop_Monoid)

instance Arbitrary (LookaheadSet Int) where
    arbitrary = liftM2 mkLookaheadSet arbitrary $ listOf arbitrary

tests :: Test
tests = testGroup "lookahead sets" [
    testProperty "monoid laws for lookahead sets"
	$ eq $ prop_Monoid (T :: T (LookaheadSet Int))
    ]
