
-- | All these different accession numbers and identifiers are confusing,
-- newtype's to the rescue.
--
-- TODO some of these names might have to change in the future...
--
-- TODO Use INT64 instead of Int...

module Biobase.Infernal.Types where

import Control.Arrow
import Data.ByteString.Char8 as BS



-- * Covariance models or Stockholm multiple alignments.

-- | The numeric identifier of a covarience model or Stockholm multiple
-- alignment as in RFxxxxx.

newtype ModelAccession = ModelAccession {unModelAccession :: Int}
  deriving (Eq,Ord,Read,Show)

-- | String identifier of a covariance model or Stockholm multiple alignment as
-- in "5S_rRNA".

newtype ModelIdentification = ModelIdentification {unModelIdentification :: ByteString}
  deriving (Eq,Ord,Read,Show)



-- * Individual sequence information

-- | EMBL sequence accession based on sequence accession and sequence start to
-- stop. (Should this then be RfamSequenceAccession?)

newtype EmblAccession = EmblAccession {unEmblAccession :: (ByteString,Int,Int)}
  deriving (Eq,Ord,Read,Show)

-- | Simple function to create 'EmblAccession' from a 'ByteString'.

mkEmblAccession :: ByteString -> EmblAccession
mkEmblAccession s = EmblAccession (sid,start,stop) where
  (sid,(Just (start,_),Just (stop,_))) = second ((BS.readInt *** (BS.readInt . BS.drop 1)) . BS.span (/='-') . BS.drop 1) . BS.span (/='/') $ s

-- | Numeric species accession number.

newtype SpeciesAccession = SpeciesAccession {unSpeciesAccession :: Int}
  deriving (Eq,Ord,Read,Show)

-- | String name for species.

newtype SpeciesName = SpeciesName {unSpeciesName :: ByteString}
  deriving (Eq,Ord,Read,Show)

-- | Strict FASTA data.

newtype StrictSeqData = StrictSeqData {unStrictSeqData :: ByteString}
  deriving (Eq,Ord,Read,Show)

