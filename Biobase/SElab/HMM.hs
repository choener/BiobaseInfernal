{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}

-- |

module Biobase.SElab.HMM where

import Data.ByteString.Char8 as BS
import Control.Lens
import Data.Word (Word32(..))

import Biobase.SElab.Types

-- | An Infernal HMM model.
--
-- TODO again, we can only parse HMMs as in Infernal 1.1. Extensions to general
-- HMMer and older versions need so follow.

data HMM = HMM
  { _version        :: (Int,Int)
  , _name           :: Identification Rfam      -- ^ the name of this HMM, tagged as 'Rfam' as these are all Rfam/HMMer models
  , _accession      :: Maybe (Accession Rfam)
  , _description    :: Maybe ByteString
  , _alph           :: ByteString
  , _rf             :: Bool
  , _consRes        :: Bool
  , _consStruc      :: Bool
  , _mapAnno        :: Bool
  , _date           :: ByteString
  , _commandLineLog :: [ByteString]
  , _nseq           :: Maybe Int              -- ^ number of sequences in multiple alignment
  , _effnseq        :: Maybe Double           -- ^ effective number of sequences (after weighting)
  , _chksum         :: Maybe Word32           -- ^ checksum (TODO: replace Word32 with actual checksum newtype)
  , _msv            :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _viterbi        :: Maybe (Double,Double)  -- ^ μ (mu) and λ (lambda) for gumbel distribution
  , _forward        :: Maybe (Double,Double)  -- ^ t (tau) and l (lambda) for exponential tails
  , _avgStateEmit   :: [BitScore]
  {-
  , _cs :: Bool
  , _alignMap :: Bool
  , _date :: ByteString
  , _symAlph :: [ByteString]
  , _transHeaders :: [ByteString]
  , _compo :: [NegLogProb]
  , _nodes :: [Node]
  -}
  } deriving (Show,Read)

makeLenses ''HMM
makePrisms ''HMM

{-

data HMM

data Alphabet
  = Amino
  | DNA
  | RNA
  | Coins
  | Dice
  | Custom
  deriving (Eq,Show,Read)

-- | Negated natural logarithm of probability.
--
-- TODO put into types stuff

newtype NegLogProb = NLP Double
  deriving (Show,Read)

-- | The nodes in an HMM. Starting with Node "0" for BEGIN.

data Node = Node
  { _nid :: Int
  , _matchE :: [NegLogProb] -- [] for BEGIN
  , _insertE :: [NegLogProb] -- insertions
  , _trans :: [NegLogProb] -- transitions: B->M1 B->I0 B->D1 I0->M1 I0->I0 0.0 * ||| Mk->Mk+1 Mk->Ik Mk->Dk+1 Ik->Mk+1 Ik->Ik Dk->Mk+1 Dk->Dk+1
  }
  deriving (Show,Read)

makeLenses ''Node


-}
