
module Main where

import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit
import Test.QuickCheck.Modifiers
import Test.QuickCheck.Property
import Control.Lens

import Biobase.SElab.HMM



-- | Import a HMMER 3 HMM and do some basic consistency checks

case_HMM_import = do
  hmms <- fromFile "tests/test.hmm"
  let h = head hmms
  assertEqual "tests/test.hmm has a single HMMER 3 HMM" 1 $ length hmms
  assertEqual "unknown lines:" mempty $ h ^. unknownLines
  assertEqual "amino alphabet" "amino" $ h ^. alph

case_CM__import = do
  return () -- assertFailure "no tests written yet"



main :: IO ()
main = $(defaultMainGenerator)

