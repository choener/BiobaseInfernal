
-- | Efficient encoding of @Infernal 1.1@ covariance models.

module Biobase.SElab.CM.Types where

import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Default
import           Data.Sequence (Seq)
import           Data.Serialize (Serialize)
import           Data.Text (Text)
import           Data.Vector.Generic (empty)
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import qualified Data.Vector as V (Vector)

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Accession
import           Data.PrimitiveArray

import           Biobase.SElab.Bitscore
import           Biobase.SElab.HMM.Types (HMM)
import           Biobase.SElab.Types



-- | Extended CM information to calculate e-values

data EValueParams = EValueParams
  { _lambda  :: Double  -- ^ λ>0 (lambda, slope) for exponential tails for local scores
  , _tau     :: Double  -- ^ τ (tau, location) for exponential tails for local scores
  , _tau2    :: Double  -- ^ τ2 (tau, location again) for full histogram of all hits
  , _dbSize  :: Int     -- ^ database size in residues
  , _numHits :: Int     -- ^ total number of non-overlapping hits
  , _tailFit :: Double  -- ^ high-scoring tail fit
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''EValueParams
makePrisms ''EValueParams

instance Default EValueParams where
  def = EValueParams
    { _lambda  = 0
    , _tau     = 0
    , _tau2    = 0
    , _dbSize  = 0
    , _numHits = 0
    , _tailFit = 0
    }

instance Binary    (EValueParams)
instance Serialize (EValueParams)
instance FromJSON  (EValueParams)
instance ToJSON    (EValueParams)



-- | @Node@s are a high-level structure in covariance models, with each
-- node having one or more states as children. In addition, nodes carry
-- alignment-column based information.

data Node = Node
  { _nstates :: Vector Int
  , _ntype   :: NodeType
  , _nid     :: Int
  , _nColL   :: Int
  , _nColR   :: Int
  , _nConL   :: Char
  , _nConR   :: Char
  , _nRefL   :: Char
  , _nRefR   :: Char
  }
  deriving (Show,Read,Generic)

makeLenses ''Node
makePrisms ''Node

instance Default Node where
  def = Node
    { _nstates  = empty
    , _ntype    = NodeType (-1)
    , _nid      = -1
    , _nColL    = -1
    , _nColR    = -1
    , _nConL    = '-'
    , _nConR    = '-'
    , _nRefL    = '-'
    , _nRefR    = '-'
    }
  {-# Inline def #-}

instance Binary    (Node)
instance Serialize (Node)
instance FromJSON  (Node)
instance ToJSON    (Node)



-- | Encode all the information necessary to have *efficient* covariance
-- models.
--
-- TODO state types
-- TODO transitions and associated costs
-- TODO emissions pair/single
-- TODO local / global mode
-- TODO add QDB information here?
--
-- TODO maybe add @_sNumTransitions@ so that we don't have to check for @-1@.
-- TODO maybe check for @0@ instead of @-1@, there is an ASM op for that
--
-- TODO We need to modify how BiobaseXNA encodes RNA sequences (maybe ACGUN)

data States = States
  { _sTransitions     :: ! (Unboxed (Z:.Int:.Int) (Int,Bitscore))              -- ^ Transitions to a state, together with the transition score; unpopulated transitions are set to @-1@.
  , _sPairEmissions   :: ! (Unboxed (Z:.Int:.Letter RNA:.Letter RNA) Bitscore) -- ^ Scores for the emission of a pair
  , _sSingleEmissions :: ! (Unboxed (Z:.Int:.Letter RNA) Bitscore)             -- ^ Scores for the emission of a single nucleotide
  , _sStateType       :: ! (Unboxed (Z:.Int) StateType)                        -- ^ Type of the state at the current index
  }
  deriving (Show,Read,Generic)

makeLenses ''States
makePrisms ''States

instance Default States where
  def = States
    { _sTransitions     = fromAssocs (Z:.0:.0)    (Z:.0:.0)    (0,0)             []
    , _sPairEmissions   = fromAssocs (Z:.0:.A:.A) (Z:.0:.A:.A) 0                 []
    , _sSingleEmissions = fromAssocs (Z:.0:.A)    (Z:.0:.A)    0                 []
    , _sStateType       = fromAssocs (Z:.0)       (Z:.0)       (StateType $ -1)  []
    }

instance Binary    (States)
instance Serialize (States)
instance FromJSON  (States)
instance ToJSON    (States)



-- | Covariance models for @Infernal@ non-coding RNA structures.

data CM = CM
  { _version        :: (Text,Text)
  , _name           :: Text
  , _accession      :: Accession Rfam
  , _description    :: Text
  , _clen           :: Int
  , _w              :: Int
  , _alph           :: Text
  , _date           :: Text
  , _commandLineLog :: Seq Text
  , _pbegin         :: Double
  , _pend           :: Double
  , _wbeta          :: Double
  , _qdbBeta1       :: Double
  , _qdbBeta2       :: Double
  , _n2Omega        :: Double
  , _n3Omega        :: Double
  , _elseLF         :: Double
  , _nseq           :: Int
  , _effn           :: Double
  , _cksum          :: Word32
  , _nullModel      :: Vector Bitscore
  , _ga             :: Double
  , _tc             :: Double
  , _efp7gf         :: (Double,Double)
  , _ecmlc          :: EValueParams
  , _ecmgc          :: EValueParams
  , _ecmli          :: EValueParams
  , _ecmgi          :: EValueParams
  , _nodes          :: V.Vector Node
  , _states         :: States
  , _hmm            :: HMM Rfam
  }
  deriving (Show,Read,Generic)

makeLenses ''CM
makePrisms ''CM

instance Default CM where
  def = CM
    { _version        = ("","")
    , _name           = ""
    , _accession      = ""
    , _description    = ""
    , _clen           = 0
    , _w              = 0
    , _alph           = ""
    , _date           = ""
    , _commandLineLog = def
    , _pbegin         = 0
    , _pend           = 0
    , _wbeta          = 0
    , _qdbBeta1       = 0
    , _qdbBeta2       = 0
    , _n2Omega        = 0
    , _n3Omega        = 0
    , _elseLF         = 0
    , _nseq           = 0
    , _effn           = 0
    , _cksum          = 0
    , _nullModel      = empty
    , _ga             = 0
    , _tc             = 0
    , _efp7gf         = (0,0)
    , _ecmlc          = def
    , _ecmgc          = def
    , _ecmli          = def
    , _ecmgi          = def
    , _nodes          = empty
    , _states         = def
    , _hmm            = def
    }

instance Binary    (CM)
instance Serialize (CM)
instance FromJSON  (CM)
instance ToJSON    (CM)

