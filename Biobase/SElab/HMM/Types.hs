
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
import Biobase.Primary

import Biobase.SElab.Bitscore



-- | Which node in the HMM are we in?

data NodeIndex

-- | An efficient encoding of Infernal HMM models. With @xfam@ phantom type
-- that will pin the type to @Pfam@ or @Rfam@ (or maybe others later).
--
-- TODO Parsing has only been tested for HMMER3 and Infernal 1.1

data HMM xfam = HMM
  { _version          :: (Text,Text)            -- ^ magic string aka @HMMER3/f@ (HMMer) or @HMMER3/i@ (Infernal), followed by the bracketed version info
  , _name             :: Text                   -- ^ the name of this HMM; for Rfam HMMs, same as CM
  , _accession        :: Accession xfam         -- ^ a la @PF01234@ or @RF01234@.
  , _description      :: Text                   -- ^ one-line free text description
  , _maxInstanceLen   :: Maybe Int              -- ^ upper on length at which an instance of the model is expected to be found
  , _alphabet         :: Text                   -- ^ alphabet type, case insensitive. We are interested in @amino@, @DNA@, @RNA@, but there are others as well.
  , _modelLength      :: Int                    -- ^ number of match states in the model
  , _referenceAnno    :: Bool                   -- ^ have we picked up reference annotation from @GC RF@ lines in Stockholm? and integrated into match states?
  , _consensusRes     :: Bool                   -- ^ valid consensus residue annotation?
  , _consensusStruc   :: Bool                   -- ^ picked up consensus annotation @@SS_cons@?
  , _alignColMap      :: Bool                   -- ^ if yes, we have map annotation in the main model annotating which multiple-alignment column a state came from
  , _modelMask        :: Bool                   -- ^ if yes, a model mask is active. Annotates columns which are set to background frequency instead of observed model frequency
  , _gatheringTh      :: Maybe (Double,Double)  -- ^ gathering thresholds @ga1@ and @ga2@
  , _trustedCutoff    :: Maybe (Double,Double)  -- ^ trusted cutoffs @tc1@ and @tc2@
  , _noiseCutoff      :: Maybe (Double,Double)  -- ^ noise cutoffs @nc1@ and @nc2@
  , _date             :: Text                   -- ^ model creation date
  , _commandLineLog   :: Seq Text               -- ^ commands to build this model
  , _nseq             :: Maybe Int              -- ^ number of sequences in multiple alignment
  , _effnseq          :: Maybe Double           -- ^ effective number of sequences (after weighting)
  , _chksum           :: Maybe Word32           -- ^ checksum (TODO: replace Word32 with actual checksum newtype)
  , _msv              :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _viterbi          :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _forward          :: Maybe (Double,Double)  -- ^ t (tau) and l (lambda) for exponential tails
  , _matchMap         :: Vector Int             -- ^ match node alignment index
  , _matchRef         :: Vector Char            -- ^ match node reference annotation
  , _matchCons        :: Vector Char            -- ^ match node consensus annotation
  , _matchScores      :: Unboxed (Z:.PInt NodeIndex:.Letter Unknown) Bitscore   -- ^
  , _insertScores     :: Unboxed (Z:.PInt NodeIndex:.Letter Unknown) Bitscore   -- ^
  , _transitionScores :: Unboxed (Z:.PInt NodeIndex:.Letter Unknown) Bitscore   -- ^
  , _unknownLines     :: Seq Text               -- ^ filled with header lines that can not be parsed
  } deriving (Show,Read,Generic)

makeLenses ''HMM
makePrisms ''HMM

instance Default (HMM xfam) where
  def = HMM
    { _version          = ("","")
    , _name             = ""
    , _accession        = ""
    , _description      = ""
    , _maxInstanceLen   = Nothing
    , _alphabet         = ""
    , _modelLength      = -1
    , _referenceAnno    = False
    , _consensusRes     = False
    , _consensusStruc   = False
    , _alignColMap      = False
    , _modelMask        = False
    , _gatheringTh      = Nothing
    , _trustedCutoff    = Nothing
    , _noiseCutoff      = Nothing
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
    , _matchScores      = fromAssocs (Z:.0:.Letter 0) (Z:.0:.Letter 0) def []
    , _insertScores     = fromAssocs (Z:.0:.Letter 0) (Z:.0:.Letter 0) def []
    , _transitionScores = fromAssocs (Z:.0:.Letter 0) (Z:.0:.Letter 0) def []
    , _unknownLines     = def
    }

instance Binary    (HMM xfam)
instance Serialize (HMM xfam)
instance FromJSON  (HMM xfam)
instance ToJSON    (HMM xfam)

