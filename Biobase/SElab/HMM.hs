{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE EmptyDataDecls #-}

-- | HMMER3 HMMs. Since we do not understand HMMER3 HMMs yet, this is actually
-- just a small ``throw-away'' parser to successfully parse Infernal 1.1 CMs.
-- The next version should have a real working parser.
--
-- TODO in the future, we should split parsing into just grabbing lines between
-- HMMER and "//" and handling in-between. We need extraction of individual
-- models and similar fun.

module Biobase.SElab.HMM where

import Data.ByteString.Char8 as BS
import Control.Lens

import Biobase.SElab.Types



data HMM

data Alphabet
  = Amino
  | DNA
  | RNA
  | Coins
  | Dice
  | Custom
  deriving (Eq,Show)

-- | Negated natural logarithm of probability.
--
-- TODO put into types stuff

newtype NegLogProb = NLP Double
  deriving (Show)

-- | The nodes in an HMM. Starting with Node "0" for BEGIN.

data Node = Node
  { _nid :: Int
  , _matchE :: [NegLogProb] -- [] for BEGIN
  , _insertE :: [NegLogProb] -- insertions
  , _trans :: [NegLogProb] -- transitions: B->M1 B->I0 B->D1 I0->M1 I0->I0 0.0 * ||| Mk->Mk+1 Mk->Ik Mk->Dk+1 Ik->Mk+1 Ik->Ik Dk->Mk+1 Dk->Dk+1
  }
  deriving (Show)

makeLenses ''Node

-- | The HMM3 data structure in ``slow mode''.

data HMM3 = HMM3
  { _version :: (ByteString,ByteString)
  , _idd :: Identification HMM
  , _acc :: Maybe (Accession HMM)
  , _description :: Maybe ByteString
  , _leng :: Int -- mandatory >0 count of match states
  , _alph :: Alphabet
  , _rf :: Bool
  , _cs :: Bool
  , _alignMap :: Bool
  , _date :: ByteString
  , _symAlph :: [ByteString]
  , _transHeaders :: [ByteString]
  , _compo :: [NegLogProb]
  , _nodes :: [Node]
  } deriving (Show)

makeLenses ''HMM3
