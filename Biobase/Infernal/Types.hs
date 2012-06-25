{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Infernal Stockholm files and covariance models, and other related files
-- use a bunch of different identifiers. We provide newtypes for more type
-- safety.
--
-- TODO Use (Bio.Core.Sequence.Offset) instead of Int for sequence info

module Biobase.Infernal.Types where

import Control.Arrow
import Data.ByteString.Char8 as BS



-- * Rfam Clans. A clan is a collection of biologically related RNA families.

-- | The 'ClanAC' is the accession number. Accession numbers start at 1 and
-- each new clan get a new 'ClanAC'.

newtype ClanAC = ClanAC {unClanAC :: Int}
  deriving (Eq,Ord,Read,Show)

-- | The 'ClanID' is the name given to the clan.

newtype ClanID = ClanID {unClanID :: ByteString}
  deriving (Eq,Ord,Read,Show)



-- * Covariance models or Stockholm multiple alignments.

-- | The numeric identifier of a covarience model or Stockholm multiple
-- alignment as in RFxxxxx.

newtype ModelAC = ModelAC {unModelAC :: Int}
  deriving (Eq,Ord,Read,Show)

-- | String identifier of a covariance model or Stockholm multiple alignment as
-- in "5S_rRNA".

newtype ModelID = ModelID {unModelID :: ByteString}
  deriving (Eq,Ord,Read,Show)



-- * Individual sequence information

-- | EMBL sequence accession based on sequence accession and sequence start to
-- stop. (Should this then be RfamSequenceAccession?)

newtype EmblAC = EmblAC {unEmblAC :: (ByteString,Int,Int)}
  deriving (Eq,Ord,Read,Show)

-- | Simple function to create 'EmblAccession' from a 'ByteString'.

mkEmblAC :: ByteString -> EmblAC
mkEmblAC s = EmblAC (sid,start,stop) where
  (sid,(Just (start,_),Just (stop,_))) = second ((BS.readInt *** (BS.readInt . BS.drop 1)) . BS.span (/='-') . BS.drop 1) . BS.span (/='/') $ s

-- | Numeric species accession number.

newtype SpeciesAC = SpeciesAC {unSpeciesAC :: Int}
  deriving (Eq,Ord,Read,Show)

-- | String name for species.

newtype SpeciesName = SpeciesName {unSpeciesName :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Strict FASTA data.

newtype StrictSeqData = StrictSeqData {unStrictSeqData :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Classification names (taxonomic classification)

newtype Classification = Classification {unClassification :: ByteString}
  deriving (Eq,Ord,Read,Show)



-- * More generic newtypes, sequence identification, etc

-- | Identifies a certain scaffold or chromosome where a hit occurs

newtype Scaffold = Scaffold {unScaffold :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Infernal bit score. Behaves like a double (deriving Num).

newtype BitScore = BitScore {unBitScore :: Double}
  deriving (Eq,Ord,Read,Show,Num)
