module Main where

import Data.CfgTests(tests)
import Test.Framework(defaultMain)

main :: IO ()
main = defaultMain [tests]


