
-- | "cmalign" provides two interesting results, bit scores of sequences
-- aligned to the model and the alignments themselves.

module Biobase.Infernal.Align where

import Data.ByteString.Char8 (ByteString)

import Biobase.Infernal.Types



-- | cmalign results, includes sequence scores if available.
--
-- TODO stockholmAlignment, should be "biostockholm" (will be set after some
-- fun iteratee tests). For now, the 'ByteString' holds everything needed to
-- parse using biostockholm.

data Align = Align
  { modelIdentification :: ModelIdentification
  , sequenceScores      :: [SequenceScore]
  , stockholmAlignment  :: ByteString
  }

-- | Individual sequence scores.
--
-- TODO avgProbability should use Probability newtype

data SequenceScore = SequenceScore
  { sequenceName      :: !(ModelAccession,ModelIdentification,EmblAccession)  -- ^ sequence name, typically RFxxxxxx;RfamID;embl-accession
  , sLength           :: !Int       -- ^ aligned sequence length
  , totalBitScore     :: !BitScore  -- ^ total alignment bitscore
  , structureBitScore :: !BitScore  -- ^ structural score part
  , avgProbability    :: !Double    -- ^
  }

