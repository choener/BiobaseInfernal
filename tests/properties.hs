
module Main where

import           Control.Lens
import           Data.Monoid (mempty)
import           Test.Framework.Providers.HUnit
import           Test.Framework.Providers.QuickCheck2
import           Test.Framework.TH
import           Test.HUnit
import           Test.QuickCheck.Modifiers
import           Test.QuickCheck.Property

import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.CM as CM



-- | Import a HMMER 3 HMM and do some basic consistency checks

case_HMM_import = do
  hmms <- HMM.fromFile "tests/test.hmm"
  let h = head hmms
  assertEqual "tests/test.hmm has a single HMMER 3 HMM" 1 $ length hmms
  assertEqual "unknown lines:" mempty $ h ^. HMM.unknownLines
  assertEqual "amino alphabet" "amino" $ h ^. HMM.alphabet

case_CM__import = do
  cms <- CM.fromFile "tests/test11.cm"
  let c = head cms
  assertEqual "tests/test.CM has a single Infernal 1.1 CM" 1 $ length cms
  assertEqual "unknown lines in CM:" mempty $ c ^. CM.unknownLines
  assertEqual "unknown lines in sub-HMM:" mempty $ c ^. CM.hmm . HMM.unknownLines
  assertEqual "RNA alphabet" "RNA" $ c ^. CM.alph



main :: IO ()
main = $(defaultMainGenerator)

