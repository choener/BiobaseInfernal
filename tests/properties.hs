
module Main where

import           Control.Lens
import           Data.Monoid (mempty)
import           Test.HUnit
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property
import           Test.Tasty
import           Test.Tasty.HUnit (testCase)
import           Test.Tasty.QuickCheck (testProperty)
import           Test.Tasty.TH

import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.CM as CM
import qualified Biobase.SElab.Model as Mdl



-- | Import a HMMER 3 HMM and do some basic consistency checks

case_HMM_import = do
  hmms <- HMM.hmmFromFile "tests/test.hmm"
  let h = head hmms
  assertEqual "tests/test.hmm has a single HMMER 3 HMM" 1 $ length hmms
  assertEqual "unknown lines:" mempty $ h ^. HMM.unknownLines
  assertEqual "amino alphabet" "amino" $ h ^. HMM.alphabet

case_CM__import = do
  -- (cms, log) <- Mdl.fromFile "tests/test11.cm" 1 (const True)
  cms <- CM.cmFromFile "tests/test11.cm"
  let c = head cms
  --assertEqual "Mdl.fromFile has empty log" "" log
  assertEqual "tests/test.CM has a single Infernal 1.1 CM" 1 $ length cms
  assertEqual "unknown lines in CM:" mempty $ c ^. CM.unknownLines
  assertEqual "unknown lines in sub-HMM:" mempty $ c ^. CM.hmm . HMM.unknownLines
  assertEqual "RNA alphabet" "RNA" $ c ^. CM.alph



main :: IO ()
main = $(defaultMainGenerator)

