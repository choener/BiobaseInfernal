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

import Data.ByteString as BS
import Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as VU
import Prelude as P
import Data.Primitive.Types

import Data.PrimitiveArray
import Data.PrimitiveArray.Unboxed.Zero
import "PrimitiveArray" Data.Array.Repa.Index

import Control.Lens

import Data.Array.Repa.Index as R
import Data.Array.Repa.Shape as R
import Data.ExtShape as R
import GHC.Base (quotInt,remInt)

import Biobase.SElab.Types



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
  deriving (Eq,Ord,Show,Read,Prim)

illegalState = StateID $ -1

-- | Certain states (IL,IR,ML,MR) emit a single nucleotide, one state emits a
-- pair (MP), other states emit nothing.

data Emits
  = EmitsSingle [(Char, BitScore)]
  | EmitsPair   [((Char,Char), BitScore)]
  | EmitNothing
  deriving (Eq,Ord,Show,Read)

-- | A single state.

data State = State
  { _stateID     :: StateID               -- ^ The ID of this state
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

data CM = CM
  { _name           :: Identification Rfam  -- ^ name of model as in "tRNA"
  , _accession      :: Accession Rfam       -- ^ RFxxxxx identification
  , _version        :: ByteString           -- ^ We can parse version 1.0 and 1.1 CMs
  , _trustedCutoff  :: BitScore             -- ^ lowest score of any seed member
  , _gathering      :: BitScore             -- ^ all scores at or above 'gathering' score are in the "full" alignment
  , _noiseCutoff    :: Maybe BitScore       -- ^ highest score NOT included as member
  , _nullModel      :: VU.Vector BitScore   -- ^ Null-model: categorical distribution on ACGU

  , _nodes  :: M.Map NodeID (NodeType,[StateID])  -- ^ each node has a set of states
  , _states :: M.Map StateID State                -- ^ each state has a type, some emit characters, and some have children

  , _unsorted       :: M.Map ByteString ByteString  -- ^ all lines that are not handled. Multiline entries are key->multi-line entry
  } deriving (Show,Read)

makeLenses ''CM


-- | Map of model names to individual CMs.

type ID2CM = M.Map (Identification Rfam) CM

-- | Map of model accession numbers to individual CMs.

type AC2CM = M.Map (Accession Rfam) CM






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

