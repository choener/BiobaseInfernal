{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
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
import Data.String
import Data.Text as T



-- * 'Accession' and string 'Identifier' with phantom types.

-- | Accession number, in the format of RFxxxxx, PFxxxxx, or CLxxxxx. We keep
-- only the Int-part. A phantom type specifies which kind of accession number
-- this is. For Species, we just have an index, it seems.

newtype Accession t = ACC {unACC :: Int}
  deriving (Eq,Ord,Read,Show)

-- | One word name for the family or clan. Phantom-typed with the correct type
-- of model. Can be a longer name for species.

newtype Identification t = IDD {unIDD :: Text}
  deriving (Eq,Ord,Read,Show)

instance IsString (Identification t) where
  fromString = IDD . T.pack

-- | Tag as being a clan.

data Clan

-- | Tag as being a Pfam model.

data Pfam

-- | Tag as being an Rfam model. Used for Stockholm and CM files.

data Rfam

-- | Species have an accession number, too.

data Species


-- | Classification names (taxonomic classification)

newtype Classification = Classification {unClassification :: Text}
  deriving (Eq,Ord,Read,Show)

