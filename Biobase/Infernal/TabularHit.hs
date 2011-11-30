
-- | Simple tabular hits as returned by Infernal.

module Biobase.Infernal.TabularHit where

import Data.ByteString.Char8 as BS

import Biobase.Infernal.Hit
import Biobase.Infernal.Types



-- | Tabular Infernal hits. See Biobase.Infernal.Hit for description of the
-- individual fields.

data TabularHit = TabularHit
  { thModel       :: !ModelIdentification
  , thTarget      :: !Scaffold
  , thTargetStart :: !Int
  , thTargetStop  :: !Int
  , thModelStart  :: !Int
  , thModelStop   :: !Int
  , thBitScore    :: !BitScore
  , thEvalue      :: !Double
  , thGCpercent   :: !Int
  } deriving (Read,Show)

-- | Generalized accessors.

instance Hit TabularHit where
  model       = thModel
  target      = thTarget
  modelStart  = thModelStart
  modelStop   = thModelStop
  targetStart = thTargetStart
  targetStop  = thTargetStop
  bitScore    = thBitScore
  evalue      = thEvalue
  gcPercent   = thGCpercent
