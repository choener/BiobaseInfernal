{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

-- | Infernal CMs.
--
-- TODO order of nucleotides? ACGU?
--
-- TODO "fastCM :: CM -> FastCM" to make a data structure that is suitable for
-- high-performance applications.

module Biobase.SElab.CM where

import           Control.Applicative
import           Control.Lens
import           Data.Array.Repa.Index as R
import           Data.Array.Repa.Shape as R
import           Data.ByteString.Char8 as BS
import           Data.Ix (Ix)
import           Data.List (genericLength)
import           Data.Map as M
import           Data.Primitive.Types
import           Data.Vector as V
import           Data.Vector.Unboxed as VU
import           Data.Word (Word32(..))
import           GHC.Base (quotInt,remInt)
import           Prelude as P

import           Data.Array.Repa.ExtShape as R

import           Biobase.SElab.Types
import           Biobase.SElab.Bitscore
import qualified Biobase.SElab.HMM as HMM



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

makeLenses ''EValueParams
makePrisms ''EValueParams

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

