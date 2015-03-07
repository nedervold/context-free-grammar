module Data.CfgTests (tests) where

import Test.Framework(Test)
import Test.Framework.Providers.HUnit(testCase)
import Test.HUnit(assertEqual)

tests :: Test
tests = testCase "dummy test" $ assertEqual "true is true"
	    	     	    	    		True
						True
