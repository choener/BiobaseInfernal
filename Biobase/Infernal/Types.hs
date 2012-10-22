{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | Infernal Stockholm files and covariance models, and other related files
-- use a bunch of different identifiers. We provide newtypes for more type
-- safety.
--
-- TODO Use (Bio.Core.Sequence.Offset) instead of Int for sequence info

module Biobase.Infernal.Types where

import Control.Arrow
import Data.ByteString.Char8 as BS
import Data.Vector.Unboxed.Base
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed as VU
import Data.Primitive.Types



-- * 'Accession' and string 'Identifier' with phantom types.

-- | Accession number, in the format of RFxxxxx, PFxxxxx, or CLxxxxx. We keep
-- only the Int-part. A phantom type specifies which kind of accession number
-- this is. For Species, we just have an index, it seems.

newtype Accession t = ACC {unACC :: Int}
  deriving (Eq,Ord,Read,Show)

-- | One word name for the family or clan. Phantom-typed with the correct type
-- of model. Can be a longer name for species.

newtype Identification t = IDD {unIDD :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Tag as being a clan.

data Clan

-- | Tag as being a Pfam model.

data Pfam

-- | Tag as being an Rfam model. Used for Stockholm and CM files.

data Rfam

-- | Species have an accession number, too.

data Species


-- | Infernal bit score. Behaves like a double (deriving Num).
--
-- Infernal users guide, p.42: log-odds score in log_2 (aka bits).
--
-- S = log_2 (P(seq|CM) / P(seq|null))

newtype BitScore = BitScore {unBitScore :: Double}
  deriving (Eq,Ord,Read,Show,Num,Prim)

deriving instance Unbox BitScore
deriving instance VGM.MVector VU.MVector BitScore
deriving instance VG.Vector VU.Vector BitScore

-- | Classification names (taxonomic classification)

newtype Classification = Classification {unClassification :: ByteString}
  deriving (Eq,Ord,Read,Show)

{-
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



-- * More generic newtypes, sequence identification, etc

-- | Identifies a certain scaffold or chromosome where a hit occurs

newtype Scaffold = Scaffold {unScaffold :: ByteString}
  deriving (Eq,Ord,Read,Show)

-}
