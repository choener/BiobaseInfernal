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

import Control.Lens
import Data.ByteString.Char8 as BS
import Data.Ix (Ix)
import Data.Map as M
import Data.Primitive.Types
import Data.Vector as V
import Data.Vector.Unboxed as VU
import GHC.Base (quotInt,remInt)
import Prelude as P
import Data.List (genericLength)

import Data.Array.Repa.Index

import Data.Array.Repa.Index as R
import Data.Array.Repa.Shape as R
import Data.Array.Repa.ExtShape as R

import Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM



-- | Encode the CM versions we can parse

data CMVersion
  = Infernal10 BS.ByteString
  | Infernal11 BS.ByteString
  deriving (Eq,Ord,Show,Read)

-- | Encode CM node types.

data NodeType
  = BIF
  | MATP
  | MATL
  | MATR
  | BEGL
  | BEGR
  | ROOT
  | END
  deriving (Eq,Ord,Enum,Show,Read)

-- | Node IDs

newtype NodeID = NodeID {unNodeID :: Int}
  deriving (Eq,Ord,Show,Read)

-- | Encode CM state types.

data StateType
  = D
  | MP
  | ML
  | MR
  | IL
  | IR
  | S
  | E
  | B
  | EL
  deriving (Eq,Ord,Enum,Show,Read)

-- | State IDs

newtype StateID = StateID {unStateID :: Int}
  deriving (Eq,Ord,Show,Read,Prim,Ix,Enum,Num)

illegalState = StateID $ -1

-- | Certain states (IL,IR,ML,MR) emit a single nucleotide, one state emits a
-- pair (MP), other states emit nothing.

data Emits
  = EmitsSingle { _single :: [(Char, BitScore)] }
  | EmitsPair   { _pair :: [(Char, Char, BitScore)] }
  | EmitNothing
  deriving (Eq,Ord,Show,Read)

makeLenses ''Emits

-- | A single state.

data State = State
  { _stateID     :: StateID               -- ^ The ID of this state
  , _nodeID      :: NodeID                -- ^ to which node does this state belong
  , _nodeType    :: NodeType              -- ^ node type for this state
  , _stateType   :: StateType             -- ^ type of the state
  , _transitions :: [(StateID,BitScore)]  -- ^ which transitions, id and bitscore
  , _emits       :: Emits                 -- ^ do we emit characters
  } deriving (Eq,Ord,Show,Read)

makeLenses ''State

-- | This is an Infernal covariance model. We have a number of blocks:
--
-- - basic information like the name of the CM, accession number, etc.
--
-- - advanced information: nodes and their states, and the states themselves.
--
-- - unsorted information from the header / blasic block
--
-- The 'CM' data structure is not suitable for high-performance applications.
--
-- - score inequalities: trusted (lowest seed score) >= gathering (lowest full
-- score) >= noise (random strings)
--
--
--
-- Local entries into the CM.
--
-- The "localBegin" lens returns a map of state id's. We either have just the
-- root node (with the "S" state), or a set of states with type: MP,ML,MR,B.
--
-- The "localEnd" lens on the other hand is the set of possible early exits
-- from the model.

data CM = CM
  { _name           :: Identification Rfam  -- ^ name of model as in "tRNA"
  , _accession      :: Accession Rfam       -- ^ RFxxxxx identification
  , _version        :: CMVersion            -- ^ We can parse version 1.0 and 1.1 CMs
  , _trustedCutoff  :: BitScore             -- ^ lowest score of any seed member
  , _gathering      :: BitScore             -- ^ all scores at or above 'gathering' score are in the "full" alignment
  , _noiseCutoff    :: Maybe BitScore       -- ^ highest score NOT included as member
  , _nullModel      :: VU.Vector BitScore   -- ^ Null-model: categorical distribution on ACGU

  , _nodes  :: M.Map NodeID (NodeType,[StateID])  -- ^ each node has a set of states
  , _states :: M.Map StateID State                -- ^ each state has a type, some emit characters, and some have children

  , _localBegin :: M.Map StateID BitScore -- ^ Entries into the CM.
  , _localEnd   :: M.Map StateID BitScore -- ^ Exits out of the CM.

  , _unsorted       :: M.Map ByteString ByteString  -- ^ all lines that are not handled. Multiline entries are key->multi-line entry
  , _hmm            :: Maybe HMM.HMM3
  } deriving (Show,Read)

makeLenses ''CM



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

