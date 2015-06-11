{- LANGUAGE FlexibleInstances #-}
{- LANGUAGE GeneralizedNewtypeDeriving #-}
{- LANGUAGE MultiParamTypeClasses #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE OverloadedStrings #-}
{- LANGUAGE PackageImports #-}
{- LANGUAGE RankNTypes #-}
{- LANGUAGE RecordWildCards #-}
{- LANGUAGE StandaloneDeriving #-}
{- LANGUAGE TemplateHaskell #-}
{- LANGUAGE TypeFamilies #-}
{- LANGUAGE TypeOperators #-}
{- LANGUAGE UndecidableInstances #-}

-- | Infernal CMs.
--
-- TODO order of nucleotides? ACGU?
--
-- TODO "fastCM :: CM -> FastCM" to make a data structure that is suitable for
-- high-performance applications.

module Biobase.SElab.CM
  ( module Biobase.SElab.CM.Types
  , module Biobase.SElab.CM.Import
  ) where

import Biobase.SElab.CM.Import
import Biobase.SElab.CM.Types



{-

import           Control.Applicative
import           Control.Lens
import           Data.Default.Class
import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           Data.Word (Word32(..))
import qualified Data.Vector as V
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary
import           Biobase.Primary.Nuc.RNA
import           Data.PrimitiveArray as PA

import           Biobase.SElab.Bitscore
import           Biobase.SElab.HMM
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
  deriving (Eq,Show,Read)

instance Default EValueParams where
  def = EValueParams
    { _lambda  = 0
    , _tau     = 0
    , _tau2    = 0
    , _dbSize  = 0
    , _numHits = 0
    , _tailFit = 0
    }

makeLenses ''EValueParams
makePrisms ''EValueParams

-- |

data Node = Node
  { _nstates :: VU.Vector Int
  , _ntype   :: NodeType
  , _nid     :: Int
  , _nColL   :: Int
  , _nColR   :: Int
  , _nConL   :: Char
  , _nConR   :: Char
  , _nRefL   :: Char
  , _nRefR   :: Char
  }
  deriving (Show)

makeLenses ''Node
makePrisms ''Node

-- |

data State = State
  { _sType        :: StateType
  , _sid          :: Int
  , _sParents     :: (Int,Int)
  , _sChildren    :: (Int,Int)
  , _sqdb         :: (Int,Int,Int,Int)
  , _transitions  :: VU.Vector Bitscore
  , _emissions    :: VU.Vector Bitscore -- emission order is ACGU or AA,AC,AG,AU, CA,CC,CG,CU, GA,GC,GG,GU, UA,UC,UG,UU
  }
  deriving (Show)

makeLenses ''State
makePrisms ''State

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
  { _sTransitions     :: ! (PA.Unboxed (Z:.Int:.Int) (Int,Bitscore))              -- ^ Transitions to a state, together with the transition score; unpopulated transitions are set to @-1@.
  , _sPairEmissions   :: ! (PA.Unboxed (Z:.Int:.Letter RNA:.Letter RNA) Bitscore) -- ^ Scores for the emission of a pair
  , _sSingleEmissions :: ! (PA.Unboxed (Z:.Int:.Letter RNA) Bitscore)             -- ^ Scores for the emission of a single nucleotide
  , _sStateType       :: ! (PA.Unboxed (Z:.Int) StateType)                        -- ^ Type of the state at the current index
  }
  deriving (Show)

makeLenses ''States
makePrisms ''States

instance Default States where
  def = States
    { _sTransitions     = PA.fromAssocs (Z:.0:.0)    (Z:.0:.0)    (0,0)             []
    , _sPairEmissions   = PA.fromAssocs (Z:.0:.A:.A) (Z:.0:.A:.A) 0                 []
    , _sSingleEmissions = PA.fromAssocs (Z:.0:.A)    (Z:.0:.A)    0                 []
    , _sStateType       = PA.fromAssocs (Z:.0)       (Z:.0)       (StateType $ -1)  []
    }

-- |
--
-- TODO need efficient scoring tables

data CM = CM
  { _version      :: (Text,Text)
  , _name         :: Text
  , _accession    :: Text
  , _description  :: Text
  , _clen         :: Int
  , _w            :: Int
  , _alph         :: Text
  , _date         :: Text
  , _commandLog   :: [Text]
  , _pbegin       :: Double
  , _pend         :: Double
  , _wbeta        :: Double
  , _qdbBeta1     :: Double
  , _qdbBeta2     :: Double
  , _n2Omega      :: Double
  , _n3Omega      :: Double
  , _elseLF       :: Double
  , _nseq         :: Int
  , _effn         :: Double
  , _cksum        :: Word32
  , _nullModel    :: VU.Vector Bitscore
  , _ga           :: Double
  , _tc           :: Double
  , _efp7gf       :: (Double,Double)
  , _ecmlc        :: EValueParams
  , _ecmgc        :: EValueParams
  , _ecmli        :: EValueParams
  , _ecmgi        :: EValueParams
  , _nodes        :: V.Vector Node
  , _states       :: States
  , _hmm          :: HMM
  }
  deriving (Show)

instance Default CM where
  def = CM
    { _version     = ("","")
    , _name        = ""
    , _accession   = ""
    , _description = ""
    , _clen        = 0
    , _w           = 0
    , _alph        = ""
    , _date        = ""
    , _commandLog  = []
    , _pbegin      = 0
    , _pend        = 0
    , _wbeta       = 0
    , _qdbBeta1    = 0
    , _qdbBeta2    = 0
    , _n2Omega     = 0
    , _n3Omega     = 0
    , _elseLF      = 0
    , _nseq        = 0
    , _effn        = 0
    , _cksum       = 0
    , _nullModel   = VU.empty
    , _ga          = 0
    , _tc          = 0
    , _efp7gf      = (0,0)
    , _ecmlc       = def
    , _ecmgc       = def
    , _ecmli       = def
    , _ecmgi       = def
    , _nodes       = V.empty
    , _states      = def
    , _hmm         = def
    }

makeLenses ''CM
makePrisms ''CM









{-

-- | State IDs

newtype StateID = StateID {unStateID :: Int}
  deriving (Eq,Ord,Show,Read,Prim,Ix,Enum,Num)

-- | Node IDs

newtype NodeID = NodeID {unNodeID :: Int}
  deriving (Eq,Ord,Show,Read)

type MapS = M.Map Char        Bitscore
type MapP = M.Map (Char,Char) Bitscore

data State
  = S  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)] }
  | D  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)] }
  | E  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)] }
  | B  { _sID :: StateID, _sNodeID :: NodeID, _sBranches :: (StateID,StateID) }
  | IL { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)], _sEmitNuc  :: MapS }
  | IR { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)], _sEmitNuc  :: MapS }
  | ML { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)], _sEmitNuc  :: MapS }
  | MR { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)], _sEmitNuc  :: MapS }
  | MP { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,Bitscore)], _sEmitPair :: MapP }
  deriving (Show,Read)

makeLenses ''State
makePrisms ''State

data NodeAnno = NodeAnno
  { _consCol :: Maybe Int
  , _consRes :: Maybe Char
  , _refAnno :: Maybe Char
  }
  deriving (Show,Read,Eq)

makeLenses ''NodeAnno
makePrisms ''NodeAnno

data Node
  = Root { _nID :: NodeID, _nStateIDs :: [StateID] }
  | Bif  { _nID :: NodeID, _nStateIDs :: [StateID] }
  | MatP { _nID :: NodeID, _nStateIDs :: [StateID], _annoL :: NodeAnno, _annoR :: NodeAnno }
  | MatL { _nID :: NodeID, _nStateIDs :: [StateID], _annoL :: NodeAnno                     }
  | MatR { _nID :: NodeID, _nStateIDs :: [StateID],                     _annoR :: NodeAnno }
  | BegL { _nID :: NodeID, _nStateIDs :: [StateID] }
  | BegR { _nID :: NodeID, _nStateIDs :: [StateID] }
  | End  { _nID :: NodeID, _nStateIDs :: [StateID] }
  deriving (Show,Read)

makeLenses ''Node
makePrisms ''Node

-- | Focus on a node based on the annotated left column (either a MatP or a
-- MatL).
--
-- TODO we should be able to somehow predict the approximate start point?! And
-- use logarithmic time to find the right node.

nodeColL = undefined

-- | Focus on a node base on the annotated right column (either a MatP or a
-- MatR).

nodeColR = undefined

-- | An Infernal v1.1 covariance model.
--
-- TODO add @instance Default@
--
-- TODO add different data constructors for the different versions of CMs

data CM = CM
  -- basic information
  { _name           :: Identification Rfam    -- ^ name of model as in "tRNA"
  , _accession      :: Maybe (Accession Rfam) -- ^ RFxxxxx identification
  , _version        :: (Int,Int)              -- ^ We can parse version 1.0 and 1.1 CMs
  -- three possible cutoff scores
  , _trustedCutoff  :: Maybe Bitscore         -- ^ lowest score of any seed member
  , _gathering      :: Maybe Bitscore         -- ^ all scores at or above '_gathering' score are in the full alignment
  , _noiseCutoff    :: Maybe Bitscore         -- ^ highest score NOT included as member
  -- other info for calculations
  , _nullModel      :: VU.Vector Bitscore     -- ^ Null-model: categorical distribution on ACGU
  , _w              :: Int                    -- ^ maximum expected hit size (and thereby window length for scanning)
  , _alph           :: ByteString             -- ^ the alphabet that was used (only ACGU is currently supported)
  , _pBegin         :: Double                 -- ^ local begin probability
  , _pEnd           :: Double                 -- ^ local exit probability
  , _wBeta          :: Double                 -- ^ tail loss probability; window length
  , _qdbBeta1       :: Double                 -- ^ tail loss probability; for tight qdb
  , _qdbBeta2       :: Double                 -- ^ tail loss probability; for loose qdb
  , _n2Omega        :: Double                 -- ^ prior prob for alternative null2
  , _n3Omega        :: Double                 -- ^ prior prob for alternative null3
  , _elSelf         :: Double                 -- ^ end locally self transition score
  , _efp7gf         :: (Double,Double)        -- ^ τ (tau) and λ (lambda) for glocal forward hmm filtering
  , _ecmLC          :: Maybe EValueParams     -- ^ local CYK evalues
  , _ecmGC          :: Maybe EValueParams     -- ^ glocal CYK evalues
  , _ecmLI          :: Maybe EValueParams     -- ^ local Inside evalues
  , _ecmGI          :: Maybe EValueParams     -- ^ glocal Inside evalues
  -- annotation, backmapping, executed command lines
  , _rf             :: Bool                   -- ^ 'True' if we have a valid reference annotation in the alignment
  , _cons           :: Bool                   -- ^ 'True' if we have a valid consensus residue annotation in the alignment
  , _mapAnno        :: Bool                   -- ^ if 'True' the map annotation field maps to the multiple alignment column
  , _date           :: ByteString             -- ^ date model was constructed
  , _commandLineLog :: [ByteString]           -- ^ log of executed command lines
  , _nseq           :: Maybe Int              -- ^ number of sequences in multiple alignment
  , _effnseq        :: Maybe Double           -- ^ effective number of sequences (after weighting)
  , _chksum         :: Maybe Word32           -- ^ checksum (TODO: replace Word32 with actual checksum newtype)
  -- the actual CM in nodes and states
  , _nodes  :: M.Map NodeID Node    -- ^ each node has a set of states
  , _states :: M.Map StateID State  -- ^ each state has a type, some emit characters, and some have children
  -- these maps are non-null if we go local
  , _localBegin :: M.Map StateID Bitscore -- ^ Entries into the CM.
  , _localEnd   :: M.Map StateID Bitscore -- ^ Exits out of the CM.
  -- and finally the attached HMM
  , _hmm            :: Maybe HMM.HMM
  } deriving (Show,Read)

makeLenses ''CM
makePrisms ''CM

{-

-- | Map of model names to individual CMs.

type ID2CM = M.Map (Identification Rfam) CM

-- | Map of model accession numbers to individual CMs.

type AC2CM = M.Map (Accession Rfam) CM

-- | Make a CM have local start/end behaviour, with "pbegin" and "pend"
-- probabilities given.

makeLocal :: Double -> Double -> CM -> CM
makeLocal pbegin pend cm = makeLocalEnd pend $ makeLocalBegin pbegin cm

-- | Insert all legal local beginnings, disable root node (and root states).
-- The 'pbegin' probability the the total probability for local begins. The
-- remaining "1-pbegin" is the probability to start with node 1.

makeLocalBegin :: Double -> CM -> CM
makeLocalBegin pbegin cm = cm{_localBegin = lb} where
  lb = M.fromList . P.map (\s -> (s^.stateID, if s^.nodeID==NodeID 1 then prob2Score 1 (1-pbegin) else prob2Score 1 (pbegin/l))) $ ss
  l  = genericLength ss
  ss = P.filter (\s -> s^.stateType `P.elem` [MP,ML,MR,B]) . M.elems $ cm ^. states

-- | Insert all legal local ends.

makeLocalEnd :: Double -> CM -> CM
makeLocalEnd pend cm = cm{_localEnd = le} where
  le = M.fromList . P.map (\s -> (s^.stateID, prob2Score 1 (pend/l))) $ ss
  l  = genericLength ss
  ss = P.filter (\s -> s^.stateType `P.elem` [MP,MP,MR,S] && s^.nodeType/=ROOT && notEnding s) . M.elems $ cm^.states
  -- no local end, if the next node ends anyway
  notEnding s = not . P.any (==E) . P.map ((^.stateType) . ((cm^.states) M.!) . fst) $ s^.transitions



-- Instances

instance Shape sh => Shape (sh:.StateID) where

  rank (sh:._) = rank sh + 1
  {-# INLINE rank #-}

  zeroDim = zeroDim :. (StateID 0)
  {-# INLINE zeroDim #-}

  unitDim = unitDim :. (StateID 1)
  {-# INLINE unitDim #-}

  intersectDim (sh1 :. StateID n1) (sh2 :. StateID n2) = intersectDim sh1 sh2 :. StateID (min n1 n2)
  {-# INLINE intersectDim #-}

  addDim (sh1 :. StateID n1) (sh2 :. StateID n2) = addDim sh1 sh2 :. StateID (n1+n2)
  {-# INLINE addDim #-}

  size (sh :. StateID n) = R.size sh * n
  {-# INLINE size #-}

  sizeIsValid (sh :. StateID n)
    | R.size sh > 0 = n <= maxBound `div` R.size sh
    | otherwise = False
  {-# INLINE sizeIsValid #-}

  toIndex (sh1 :. StateID n1) (sh2 :. StateID n2) = toIndex sh1 sh2 * n1 + n2
  {-# INLINE toIndex #-}

  fromIndex (ds :. StateID d) n = fromIndex ds (n `quotInt` d) :. StateID r where
    r | rank ds == 0 = n
      | otherwise    = n `remInt` d
  {-# INLINE fromIndex #-}

  inShapeRange (sh1 :. StateID n1) (sh2 :. StateID n2) (zs :. StateID z) = (z >= n1) && (z < n2) && inShapeRange sh1 sh2 zs
  {-# INLINE inShapeRange #-}

  listOfShape (sh :. StateID n) = n : listOfShape sh
  {-# INLINE listOfShape #-}

  shapeOfList xx
    = case xx of
        []     -> error $ "shapeOfList empty in StateID"
        (x:xs) -> shapeOfList xs :. StateID x
  {-# INLINE shapeOfList #-}

  deepSeq (sh :. n) x = deepSeq sh (n `seq` x)
  {-# INLINE deepSeq #-}



instance ExtShape sh => ExtShape (sh:.StateID) where

  subDim (sh1 :. StateID n1) (sh2 :. StateID n2) = subDim sh1 sh2 :. StateID (n1-n2)
  {-# INLINE subDim #-}

  rangeList (sh1 :. StateID n1) (sh2 :. StateID n2) = [sh :. StateID n | sh <- rangeList sh1 sh2, n <- [n1 .. (n1+n2)] ]
  {-# INLINE rangeList #-}

-}

-}

-}

