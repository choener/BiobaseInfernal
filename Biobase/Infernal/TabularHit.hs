
-- | Simple tabular hits as returned by Infernal.

module Biobase.Infernal.TabularHit where

import Data.ByteString.Char8 as BS


data TabularHit = TabularHit
  { thModel :: ByteString
  , thTarget :: ByteString
  , thScaffoldStart :: Int
  , thScaffoldStop :: Int
  , thQueryStart :: Int
  , thQueryStop :: Int
  , thBitScore :: Double
  , thEvalue :: Double
  , thGCpercent :: Int
  } deriving (Read,Show)
