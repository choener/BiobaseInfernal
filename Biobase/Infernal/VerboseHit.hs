{-# LANGUAGE RecordWildCards #-}

-- | Provides a datatype for cmsearch verbose output.

module Biobase.Infernal.VerboseHit where

import qualified Data.ByteString.Char8 as BS
import Text.Printf



-- | Captures a complete alignment

data VerboseHit = VerboseHit
  { vhTarget  :: (Int,Int)  -- ^ part of target sequence (start counting at 1)
  , vhQuery   :: (Int,Int)  -- ^ which part of the CM/stk do we align to
  , vhCM      :: BS.ByteString  -- ^ the CM for this alignment
  , vhStrand  :: Strand     -- ^ should be either '+' or '-'
  , vhScore   :: Double     -- ^ bit score
  , vhEvalue  :: Double     -- ^ number of hits we expect to find with 'score' or higher for 'targetSequence' length
  , vhPvalue  :: Double     -- ^ ?
  , vhGC      :: Int        -- ^ ?
  , vhScaffold :: BS.ByteString -- ^ scaffold, chromosome, ... (the name of the sequence, not the sequence data!)
  , vhWuss :: BS.ByteString -- ^ fancy secondary structure annotation using wuss notation
  , vhConsensus :: BS.ByteString -- ^ query consensus (upper: highly, lower: weak/no)
  , vhScoring   :: BS.ByteString -- ^ represents where positive and negative scores come from
  , vhSequence :: BS.ByteString -- ^ the target sequence which aligns to the model
  } deriving (Show,Read)

type Strand = Char

