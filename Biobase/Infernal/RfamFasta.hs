{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | The Rfam.fasta.gz file provides useful information: (1) conversion between
-- Rfam accession and Rfam identifier, (2) species accession, (3) name of said
-- species, and (4) the sequence fasta file.

module Biobase.Infernal.RfamFasta where

import Bio.Core.Sequence
import Data.ByteString.Char8 as BS
import Data.Map as M
import qualified Data.ByteString.Lazy.Char8 as BSL
import Text.Printf

import Biobase.Infernal.Types



-- | Rfam FASTA entry.

data RfamFasta = RfamFasta
  { modelAccession    :: !ModelAC      -- ^ Rfam accession number RFxxxxx (the xxxxx part).
  , modelIdentifier   :: !ModelID -- ^ Rfam identifier (like 5S_rRNA).
  , sequenceAccession :: !EmblAC       -- ^ EMBL sequence accession identifier and position.
  , speciesAccession  :: !SpeciesAC    -- ^ Rfam species accession.
  , speciesName       :: !SpeciesName         -- ^ Species name.
  , fastaData         :: !StrictSeqData       -- ^ FASTA data
  } deriving (Show)

-- | Since RfamFasta entries are just fasta entries...

instance BioSeq RfamFasta where
  seqlabel RfamFasta{..}  = SeqLabel . BSL.fromChunks $ [BS.concat
    [ BS.pack . printf "RF%05d" . unModelAC $ modelAccession
    , ";"
    , unModelID modelIdentifier
    , ";"
    , let (a,b,c) = unEmblAC sequenceAccession in BS.concat [a, "/", BS.pack $ show b, "-", BS.pack $ show c]
    , "   "
    , BS.pack . show . unSpeciesAC $ speciesAccession
    , ":"
    , unSpeciesName speciesName
    ] ]
  seqdata RfamFasta{..}   = SeqData . BSL.fromChunks $ [unStrictSeqData fastaData]
  seqlength RfamFasta{..} = Offset . fromInteger . toInteger . BS.length . unStrictSeqData $ fastaData



-- * Some in-memory lookup systems.

-- | Model accession to model identifier

type ModelAC2ID = Map ModelAC ModelID

-- | Model identifier to model accession

type ModelID2AC = Map ModelID ModelAC

-- | Model accession and sequence accession to 'RfamFasta' entry (and model
-- accession to all entries for this accession).

type ACAC2RfamFasta = Map ModelAC (Map EmblAC RfamFasta)

-- | Model identifier and sequence accession to 'RfamFasta' entry.

type IDAC2RfamFasta = Map ModelID (Map EmblAC RfamFasta)

