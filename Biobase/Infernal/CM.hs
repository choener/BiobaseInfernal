{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PackageImports #-}

-- | Infernal CMs.
--
-- TODO order of nucleotides? ACGU?
--
-- TODO "fastCM :: CM -> FastCM" to make a data structure that is suitable for
-- high-performance applications.

module Biobase.Infernal.CM where

import Data.ByteString as BS
import Data.Map as M
import Data.Vector as V
import Data.Vector.Unboxed as VU
import Prelude as P

import Data.PrimitiveArray
import Data.PrimitiveArray.Unboxed.Zero
import "PrimitiveArray" Data.Array.Repa.Index

import Biobase.Infernal.Types

import Data.Lens.Common
import Data.Lens.Template



-- | Encode CM node types.

newtype NodeType = NodeType {unNodeType :: Int}
  deriving (Eq,Ord,Show)

(nBIF:nMATP:nMATL:nMATR:nBEGL:nBEGR:nROOT:nEND:_) = P.map NodeType [0..]

nodeTypeFromString :: String -> NodeType
nodeTypeFromString = f where
  f "BIF"  = nBIF
  f "MATP" = nMATP
  f "MATL" = nMATL
  f "MATR" = nMATR
  f "BEGL" = nBEGL
  f "BEGR" = nBEGR
  f "ROOT" = nROOT
  f "END"  = nEND
  f xs     = error $ "unknown node type: " P.++ xs

-- | Node IDs

newtype NodeID = NodeID {unNodeID :: Int}
  deriving (Eq,Ord,Show)

-- | Encode CM state types.

newtype StateType = StateType {unStateType :: Int}
  deriving (Eq,Ord,Show)

(sD:sMP:sML:sMR:sIL:sIR:sS:sE:sB:sEL:_) = P.map StateType [0..]

-- | State IDs

newtype StateID = StateID {stateID :: Int}
  deriving (Eq,Ord,Show)

-- | Certain states (IL,IR,ML,MR) emit a single nucleotide, one state emits a
-- pair (MP), other states emit nothing.

data Emits
  = EmitsSingle [(Char, BitScore)]
  | EmitsPair   [((Char,Char), BitScore)]
  | EmitNothing
  deriving (Eq,Ord,Show)

-- | This is an Infernal covariance model. We have a number of blocks:
--
-- - basic information like the name of the CMA
--
-- - advanced information: nodes, transitions between states, emissions for
-- some states
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

  , _nodes        :: M.Map NodeID (NodeType,[StateID])             -- ^ each node has a set of states
  , _transitions  :: M.Map StateID [(StateType,BitScore)] -- ^ transition scores. Given a state, what are reachable states and the transition cost
  , _emissions    :: M.Map StateID Emits              -- ^ Given a state, what is emitted: a single nucleotide, a pair of nucleotides, or nothing

  , _unsorted       :: M.Map ByteString ByteString  -- ^ all lines that are not handled. Multiline entries are key->multi-line entry
  }

$( makeLens ''CM )


-- A datatype representing Infernal covariance models. This is a new
-- representation that is incompatible with the one once found in "Biobase".
-- The most important difference is that lookups are mapped onto efficient data
-- structures, currently "PrimitiveArray".
--
-- [1] Each "State" of a covariance model has up to 6 transition scores, hence
-- we need s*6 cells for transitions.
--
-- [2] Each "State" of a covariance has up to 16 emission scores, so we have
-- s*16 cells for emissions, with unused cells set to a really high score.
--
-- On top of these basic structures, we then place additional high-level
-- constructs.
--
-- [3] 'paths' are allowed transitions. This can safe a check, if the
-- transition is encoded with a forbidden score.
--
-- [4] 'localBegin' and 'localEnd' are local entry and exit strategies. A
-- 'localBegin' is a transition score to certain states, all such transitions
-- are in 'begins'. A 'localEnd' is a transition score to a local end state.
--
-- NOTE that trustedCutoff > gathering > noiseCutoff
--
-- TODO as with other projects, we should not use Double's but "Score" and
-- "Probability" newtypes.



-- | Map of model names to individual CMs.

type ID2CM = M.Map (Identification Rfam) CM

-- | Map of model accession numbers to individual CMs.

type AC2CM = M.Map (Accession Rfam) CM

