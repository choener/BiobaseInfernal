
-- | Infernal Stockholm files and covariance models, and other related files
-- use a bunch of different identifiers. We provide newtypes for more type
-- safety.
--
-- TODO Use (Bio.Core.Sequence.Offset) instead of Int for sequence info

module Biobase.SElab.Types where

import           Control.Applicative
import           Control.Arrow ()
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable)
import           Data.Serialize
import           Data.Serialize.Text
import           Data.String
import           Data.Text.Binary
import           Data.Text (Text)
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T



-- * 'Accession' and string 'Identifier' with phantom types.

newtype Identification t = Identification Text
  deriving (Eq,Ord,Read,Show,Generic)

instance Binary    (Identification t)
instance FromJSON  (Identification t)
instance Hashable  (Identification t)
instance Serialize (Identification t)
instance ToJSON    (Identification t)

instance IsString (Identification t) where
  fromString = Identification . T.pack



-- | Classification names (taxonomic classification)

newtype Classification = Classification Text
  deriving (Eq,Ord,Read,Show,Generic)

instance Binary    Classification
instance FromJSON  Classification
instance Hashable  Classification
instance Serialize Classification
instance ToJSON    Classification

instance IsString Classification where
  fromString = Classification . T.pack



