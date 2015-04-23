{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Cfg.FreeCfgTests (
    tests
    ) where

import Data.Cfg.Bnf(bnf)
import Data.Cfg.Cfg(Cfg(..), V(..), eqCfg, productions)
import Data.Cfg.FreeCfg
import Data.Cfg.FreeCfgInstances()
import Data.Cfg.TestGrammars(assertEqCfg)
import qualified Data.Set as S
import Test.Framework(Test, testGroup)
import Test.Framework.Providers.HUnit(testCase)
import Test.Framework.Providers.QuickCheck2(testProperty)
import Text.PrettyPrint

tests :: Test
tests = testGroup "Data.Cfg.FreeCfg" [
    bimapCfgTest,
    bimapCfgProp
    ]

bimapCfgTest :: Test
bimapCfgTest = testCase "bimapCfg works properly" $
    assertEqCfg ctxt ctxt "bimapCfg result is correct" expected cfg'
    where
    cfg = toFreeCfg [bnf|
		a ::= A b.
		b ::= b B | .
		|]

    cfg' :: FreeCfg String String
    cfg' = bimapCfg (++ "_TERMINAL") ("nt_" ++) cfg

    expected = toFreeCfg [bnf|
		   nt_a ::= A_TERMINAL nt_b .
		   nt_b ::= nt_b B_TERMINAL | . |]
    ctxt v = text $ case v of
			NT nt -> nt
			T t -> t

bimapCfgProp :: Test
bimapCfgProp = testProperty "bimapCfg id id == removeUnusedTerminals" f
    where
    f :: FreeCfg Int Int -> Bool
    f cfg = eqCfg (removeUnusedTerminals cfg) (bimapCfg id id cfg)

removeUnusedTerminals :: forall cfg nt t
		      . (Cfg cfg t nt, Ord t)
		      => cfg t nt -> FreeCfg t nt
removeUnusedTerminals cfg = (toFreeCfg cfg) {
    terminals' = usedTerminals
    }

    where
    vs :: [V t nt]
    vs = concat [ NT hd : rhs | (hd, rhs) <- productions cfg ]

    usedTerminals :: S.Set t
    usedTerminals = S.fromList [ t | T t <- vs ]