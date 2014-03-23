{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}

-- |

module Biobase.SElab.HMM where

import           Control.Lens
import           Data.Array.Repa.Index
import           Data.Default.Class
import           Data.Text
import           Data.Word (Word32(..))
import qualified Data.PrimitiveArray as PA
import qualified Data.PrimitiveArray.Zero as PA
import qualified Data.Vector.Unboxed as VU

import           Biobase.SElab.Bitscore
import           Biobase.SElab.Types



-- | An efficient encoding of Infernal HMM models.
--
-- TODO Parsing has only been tested for HMMER3 and Infernal 1.1

data HMM = HMM
  { _version          :: (Int,Int,Text)
  , _name             :: Text -- ^ the name of this HMM, tagged as 'Rfam' as these are all Rfam/HMMer models
  , _accession        :: Text
  , _description      :: Text
  , _alph             :: Text
  , _rf               :: Bool
  , _cs               :: Bool
  , _consRes          :: Bool
  , _consStruc        :: Bool
  , _mapAnno          :: Bool
  , _date             :: Text
  , _commandLineLog   :: [Text]
  , _nseq             :: Maybe Int              -- ^ number of sequences in multiple alignment
  , _effnseq          :: Maybe Double           -- ^ effective number of sequences (after weighting)
  , _chksum           :: Maybe Word32           -- ^ checksum (TODO: replace Word32 with actual checksum newtype)
  , _msv              :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _viterbi          :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _forward          :: Maybe (Double,Double)  -- ^ t (tau) and l (lambda) for exponential tails
  , _matchMap         :: VU.Vector Int          -- ^ match node alignment index
  , _matchRef         :: VU.Vector Char         -- ^ match node reference annotation
  , _matchCons        :: VU.Vector Char         -- ^ match node consensus annotation
  , _matchScores      :: PA.Unboxed (Z:.Int:.Int) Bitscore
  , _insertScores     :: PA.Unboxed (Z:.Int:.Int) Bitscore
  , _transitionScores :: PA.Unboxed (Z:.Int:.Int) Bitscore
  } deriving (Show,Read)

makeLenses ''HMM
makePrisms ''HMM

instance Default HMM where
  def = HMM
    { _version          = (0,0,"")
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
    , _commandLineLog   = []
    , _nseq             = Nothing
    , _effnseq          = Nothing
    , _chksum           = Nothing
    , _msv              = Nothing
    , _viterbi          = Nothing
    , _forward          = Nothing
    , _matchMap         = VU.empty
    , _matchRef         = VU.empty
    , _matchCons        = VU.empty
    , _matchScores      = PA.fromAssocs (Z:.0:.0) (Z:.0:.0) 999999 []
    , _insertScores     = PA.fromAssocs (Z:.0:.0) (Z:.0:.0) 999999 []
    , _transitionScores = PA.fromAssocs (Z:.0:.0) (Z:.0:.0) 999999 []
    }

