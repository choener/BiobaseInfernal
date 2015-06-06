
module Main where

import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property



-- TODO add a hunit test (or s.th. like that) that reads a HMM and checks
-- it for sanity.

main :: IO ()
main = $(defaultMainGenerator)

