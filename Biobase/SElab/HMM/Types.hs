
-- | Efficient encoding of Hidden-Markov models as used by @HMMER 3@ and
-- @Infernal 1.1@.

module Biobase.SElab.HMM.Types where

import Control.Lens
import Data.Aeson (FromJSON,ToJSON)
import Data.Binary (Binary)
import Data.Default
import Data.PrimitiveArray
import Data.Sequence (Seq)
import Data.Serialize (Serialize)
import Data.Text (Text)
import Data.Vector.Unboxed (Vector,empty)
import Data.Word (Word32)
import GHC.Generics (Generic)

import Data.PrimitiveArray
import Biobase.Types.Accession

import Biobase.SElab.Bitscore



-- | An efficient encoding of Infernal HMM models.
--
-- TODO Parsing has only been tested for HMMER3 and Infernal 1.1

data HMM xfam = HMM
  { _version          :: (Text,Text)            -- ^ magic string aka @HMMER3/f@ (HMMer) or @HMMER3/i@ (Infernal), followed by the bracketed version info
  , _name             :: Text                   -- ^ the name of this HMM, tagged as 'Rfam' as these are all Rfam/HMMer models
  , _accession        :: Accession xfam
  , _description      :: Text
  , _alph             :: Text
  , _rf               :: Bool
  , _cs               :: Bool
  , _consRes          :: Bool
  , _consStruc        :: Bool
  , _mapAnno          :: Bool
  , _date             :: Text
  , _commandLineLog   :: Seq Text
  , _nseq             :: Maybe Int              -- ^ number of sequences in multiple alignment
  , _effnseq          :: Maybe Double           -- ^ effective number of sequences (after weighting)
  , _chksum           :: Maybe Word32           -- ^ checksum (TODO: replace Word32 with actual checksum newtype)
  , _msv              :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _viterbi          :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _forward          :: Maybe (Double,Double)  -- ^ t (tau) and l (lambda) for exponential tails
  , _matchMap         :: Vector Int             -- ^ match node alignment index
  , _matchRef         :: Vector Char            -- ^ match node reference annotation
  , _matchCons        :: Vector Char            -- ^ match node consensus annotation
  , _matchScores      :: Unboxed (Z:.Int:.Int) Bitscore
  , _insertScores     :: Unboxed (Z:.Int:.Int) Bitscore
  , _transitionScores :: Unboxed (Z:.Int:.Int) Bitscore
  , _unknownLines     :: Seq Text               -- ^ filled with lines that can not be parsed
  } deriving (Show,Read,Generic)

makeLenses ''HMM
makePrisms ''HMM

instance Default (HMM xfam) where
  def = HMM
    { _version          = ("","")
    , _name             = ""
    , _accession        = ""
    , _description      = ""
    , _alph             = ""
    , _rf               = False
    , _cs               = False
    , _consRes          = False
    , _consStruc        = False
    , _mapAnno          = False
    , _date             = ""
    , _commandLineLog   = def
    , _nseq             = Nothing
    , _effnseq          = Nothing
    , _chksum           = Nothing
    , _msv              = Nothing
    , _viterbi          = Nothing
    , _forward          = Nothing
    , _matchMap         = empty
    , _matchRef         = empty
    , _matchCons        = empty
    , _matchScores      = fromAssocs (Z:.0:.0) (Z:.0:.0) def []
    , _insertScores     = fromAssocs (Z:.0:.0) (Z:.0:.0) def []
    , _transitionScores = fromAssocs (Z:.0:.0) (Z:.0:.0) def []
    , _unknownLines     = def
    }

instance Binary    (HMM xfam)
instance Serialize (HMM xfam)
instance FromJSON  (HMM xfam)
instance ToJSON    (HMM xfam)

