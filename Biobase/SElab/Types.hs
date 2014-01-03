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
--
-- TODO move 'BitScore's, null models, probabilities into its own library.

module Biobase.SElab.Types where

import Control.Arrow
import Data.ByteString.Char8 as BS
import Data.Vector.Unboxed.Base
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed as VU
import Data.Primitive.Types
import Data.Default.Class



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

-- | A default bitscore of "-infinity".
--
-- TODO Check out the different "defaults" Infernal uses

instance Default BitScore where
  def = BitScore (-999999)

deriving instance Unbox BitScore
deriving instance VGM.MVector VU.MVector BitScore
deriving instance VG.Vector VU.Vector BitScore

-- | Given a null model and a probability, calculate the corresponding
-- 'BitScore'.

prob2Score :: Double -> Double -> BitScore
prob2Score null x
  | x==0      = BitScore $ -10000
  | otherwise = BitScore $ log (x/null) / log 2
{-# INLINE prob2Score #-}

-- | Given a null model and a 'BitScore' return the corresponding probability.

score2Prob :: Double -> BitScore -> Double
score2Prob null (BitScore x)
  | x<=(-9999) = 0
  | otherwise  = null * exp (x * log 2)
{-# INLINE score2Prob #-}

-- | Classification names (taxonomic classification)

newtype Classification = Classification {unClassification :: ByteString}
  deriving (Eq,Ord,Read,Show)

