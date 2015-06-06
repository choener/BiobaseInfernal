
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
import           Data.Ix (Ix)
import           Data.Serialize
import           Data.Serialize.Text
import           Data.String
import           Data.Text.Binary
import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Text as T
import           Text.Read



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



-- * Special types for CMs

-- | The type of a node, efficiently encoded as an Int.
--
-- TODO we might want a nice read instance

newtype NodeType = NodeType Int
  deriving (Eq,Ord,Generic,Ix)

pattern Bif  = NodeType 0
pattern MatP = NodeType 1
pattern MatL = NodeType 2
pattern MatR = NodeType 3
pattern BegL = NodeType 4
pattern BegR = NodeType 5
pattern Root = NodeType 6
pattern End  = NodeType 7

instance Binary    NodeType
instance FromJSON  NodeType
instance Hashable  NodeType
instance Serialize NodeType
instance ToJSON    NodeType

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
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "BIF"  -> Bif
      "MATP" -> MatP
      "MATL" -> MatL
      "MATR" -> MatR
      "BEGL" -> BegL
      "BEGR" -> BegR
      "ROOT" -> Root
      "END"  -> End
      _      -> error $ "read NodeType: " ++ s

derivingUnbox "NodeType"
  [t| NodeType -> Int |] [| \(NodeType n) -> n |] [| NodeType |]

-- | Type of a state, a newtype wrapper for performance

newtype StateType = StateType Int
  deriving (Eq,Ord,Generic,Ix)

pattern D  = StateType 0
pattern MP = StateType 1
pattern ML = StateType 2
pattern MR = StateType 3
pattern IL = StateType 4
pattern IR = StateType 5
pattern S  = StateType 6
pattern E  = StateType 7
pattern B  = StateType 8
pattern EL = StateType 9

instance Binary    StateType
instance FromJSON  StateType
instance Hashable  StateType
instance Serialize StateType
instance ToJSON    StateType

instance Show StateType where
  show = \case
    D  -> "D"
    MP -> "MP"
    ML -> "ML"
    MR -> "MR"
    IL -> "IL"
    IR -> "IR"
    S  -> "S"
    E  -> "E"
    B  -> "B"
    EL -> "EL"

instance Read StateType where
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "D"  -> D
      "MP" -> MP
      "ML" -> ML
      "MR" -> MR
      "IL" -> IL
      "IR" -> IR
      "S"  -> S
      "E"  -> E
      "B"  -> B
      "EL" -> EL
      _    -> error $ "read StateType: " ++ s

derivingUnbox "StateType"
  [t| StateType -> Int |] [| \(StateType s) -> s |] [| StateType |]

