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

import Control.Applicative
import Control.Lens
import Data.Array.Repa.Index as R
import Data.Array.Repa.Shape as R
import Data.ByteString.Char8 as BS
import Data.Ix (Ix)
import Data.List (genericLength)
import Data.Map as M
import Data.Primitive.Types
import Data.Vector as V
import Data.Vector.Unboxed as VU
import GHC.Base (quotInt,remInt)
import Prelude as P

import Data.Array.Repa.ExtShape as R

import Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM



-- | State IDs

newtype StateID = StateID {unStateID :: Int}
  deriving (Eq,Ord,Show,Read,Prim,Ix,Enum,Num)

-- | Node IDs

newtype NodeID = NodeID {unNodeID :: Int}
  deriving (Eq,Ord,Show,Read)

type MapS = M.Map Char        BitScore
type MapP = M.Map (Char,Char) BitScore

data State
  = S  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)] }
  | D  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)] }
  | E  { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)] }
  | B  { _sID :: StateID, _sNodeID :: NodeID, _sBranches :: (StateID,StateID) }
  | IL { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)], _sEmitNuc  :: MapS }
  | IR { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)], _sEmitNuc  :: MapS }
  | ML { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)], _sEmitNuc  :: MapS }
  | MR { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)], _sEmitNuc  :: MapS }
  | MP { _sID :: StateID, _sNodeID :: NodeID, _sChildren :: [(StateID,BitScore)], _sEmitPair :: MapP }
  deriving (Show,Read)

makeLenses ''State
makePrisms ''State

data Node
  = Root { _nID :: NodeID, _nStateIDs :: [StateID] }
  | Bif  { _nID :: NodeID, _nStateIDs :: [StateID] }
  | MatP { _nID :: NodeID, _nStateIDs :: [StateID] }
  | MatL { _nID :: NodeID, _nStateIDs :: [StateID] }
  | MatR { _nID :: NodeID, _nStateIDs :: [StateID] }
  | BegL { _nID :: NodeID, _nStateIDs :: [StateID] }
  | BegR { _nID :: NodeID, _nStateIDs :: [StateID] }
  | End  { _nID :: NodeID, _nStateIDs :: [StateID] }
  deriving (Show,Read)

makeLenses ''Node
makePrisms ''Node

data CM = CM
  { _name           :: Identification Rfam    -- ^ name of model as in "tRNA"
  , _accession      :: Maybe (Accession Rfam) -- ^ RFxxxxx identification
  , _version        :: (Int,Int)              -- ^ We can parse version 1.0 and 1.1 CMs
  , _trustedCutoff  :: Maybe BitScore         -- ^ lowest score of any seed member
  , _gathering      :: Maybe BitScore         -- ^ all scores at or above 'gathering' score are in the "full" alignment
  , _noiseCutoff    :: Maybe BitScore         -- ^ highest score NOT included as member
  , _nullModel      :: VU.Vector BitScore     -- ^ Null-model: categorical distribution on ACGU

  , _nodes  :: M.Map NodeID Node    -- ^ each node has a set of states
  , _states :: M.Map StateID State  -- ^ each state has a type, some emit characters, and some have children

  , _localBegin :: M.Map StateID BitScore -- ^ Entries into the CM.
  , _localEnd   :: M.Map StateID BitScore -- ^ Exits out of the CM.

  , _hmm            :: Maybe HMM.HMM3
  } deriving (Show,Read)

makeLenses ''CM

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

