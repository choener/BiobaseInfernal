{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -funbox-strict-fields #-}

-- | Provides a datatype for cmsearch verbose output. The Import/Export system
-- now allows for primitive annotations using "##" as the first two characters.
-- Annotations are only accepted for individual hits.

module Biobase.Infernal.VerboseHit where

import Data.ByteString.Char8 as BS
import Text.Printf

import Biobase.Infernal.Hit



-- | Captures a complete alignment

data VerboseHit = VerboseHit
  { vhTargetStart :: !Int          -- ^ part of target sequence (start counting at 1)
  , vhTargetStop  :: !Int
  , vhModelStart  :: !Int          -- ^ which part of the CM/stk do we align to
  , vhModelStop   :: !Int          -- ^ which part of the CM/stk do we align to
  , vhModel       :: !ByteString   -- ^ the CM for this alignment
  , vhStrand      :: !Strand       -- ^ should be either '+' or '-'
  , vhBitScore    :: !Double       -- ^ bit score
  , vhEvalue      :: !Double       -- ^ number of hits we expect to find with 'score' or higher for 'targetSequence' length
  , vhPvalue      :: !Double       -- ^ ?
  , vhGCpercent   :: !Int          -- ^ ?
  , vhTarget      :: !ByteString   -- ^ scaffold, chromosome, ... (the name of the sequence, not the sequence data!)
  , vhWuss        :: !ByteString   -- ^ fancy secondary structure annotation using wuss notation
  , vhConsensus   :: !ByteString   -- ^ query consensus (upper: highly, lower: weak/no)
  , vhScoring     :: !ByteString   -- ^ represents where positive and negative scores come from
  , vhSequence    :: !ByteString   -- ^ the target sequence which aligns to the model
  , vhAnnotation  :: ![ByteString] -- ^ any annotations that could be associated (# lines)
  } deriving (Show,Read)

type Strand = Char

-- | Generalized accessors.

instance Hit VerboseHit where
  model       = vhModel
  target      = vhTarget
  modelStart  = vhModelStart
  modelStop   = vhModelStop
  targetStart = vhTargetStart
  targetStop  = vhTargetStop
  bitScore    = vhBitScore
  evalue      = vhEvalue
  gcPercent   = vhGCpercent
