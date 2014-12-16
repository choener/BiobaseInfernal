
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDecls #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

-- | Infernal Stockholm files and covariance models, and other related files
-- use a bunch of different identifiers. We provide newtypes for more type
-- safety.
--
-- TODO Use (Bio.Core.Sequence.Offset) instead of Int for sequence info
--
-- TODO move 'BitScore's, null models, probabilities into its own library.

module Biobase.SElab.Types where

import           Control.Arrow
import           Data.Ix (Ix)
import           Data.String
import           Data.Text (Text)
import           GHC.Generics
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T



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



-- * Special types for CMs

-- | The type of a node, efficiently encoded as an Int.
--
-- TODO we might want a nice read instance

newtype NodeType = NodeType {unNodeType :: Int}
  deriving (Eq,Ord,Generic,Ix)

pattern Bif  = NodeType 0
pattern MatP = NodeType 1
pattern MatL = NodeType 2
pattern MatR = NodeType 3
pattern BegL = NodeType 4
pattern BegR = NodeType 5
pattern Root = NodeType 6
pattern End  = NodeType 7

instance Show NodeType where
  show = \case
    Bif  -> "BIF"
    MatP -> "MATP"
    MatL -> "MATL"
    MatR -> "MATR"
    BegL -> "BEGL"
    BegR -> "BEGR"
    Root -> "ROOT"
    End  -> "END"

instance Read NodeType where
  readsPrec p = \case
    (null      -> True)    -> []
    (sp " "    -> Just ys) -> readsPrec p ys
    (sp "BIF"  -> Just ys) -> [(Bif , ys)]
    (sp "MATP" -> Just ys) -> [(MatP, ys)]
    (sp "MATL" -> Just ys) -> [(MatL, ys)]
    (sp "MATR" -> Just ys) -> [(MatR, ys)]
    (sp "BEGL" -> Just ys) -> [(BegL, ys)]
    (sp "BEGR" -> Just ys) -> [(BegR, ys)]
    (sp "ROOT" -> Just ys) -> [(Root, ys)]
    (sp "END"  -> Just ys) -> [(End , ys)]
    where sp = L.stripPrefix

