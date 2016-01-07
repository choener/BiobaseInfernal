{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Data structure that hold tabularized or verbose cmsearch hits.
--
-- TODO maybe start to have type classes for stuff like @accession@ lenses?
--
-- TODO cmscan probably wants to return 'Hit's as well.
--
-- TODO would be "kind-of useful" to have a method that extends tabular
-- hits to verbose hits. Probably by calling cmalign with the extracted
-- sequence piece.

module Biobase.SElab.Hit.Hit
  ( module Biobase.SElab.Hit.Alignment
  , module Biobase.SElab.Hit.Hit
  ) where

import           Bio.Core
import           Control.Lens
import           Data.Text (Text)
import           GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU

import           Biobase.SElab.Bitscore
import           Biobase.SElab.Hit.Alignment



-- | Indicate wether a hit is truncated on the 5', 3', no, or both sides.

type Truncated = (Bool , Bool) 

-- | Sequence position as 64 bit offset pairs.

type FromTo = (Offset , Offset)

-- | Alignments are unboxed vectors of columns.
--
-- TODO would be nice to make compatible with generic alignment library.

type Alignment = VU.Vector Column

-- | Tabular or verbose hit. Verbose hits carry an alignment in addition to
-- the hit information.

data Hit = Hit
  { _targetName   :: Text       -- ^ name of target sequence
  , _targetDesc   :: Text       -- ^ target sequence description
  , _targetAcc    :: Text       -- ^ target accesion or @-@ if none (should be use @""@ for none?)
  , _queryName    :: Text       -- ^ query name
  , _queryAcc     :: Text       -- ^ query accession or @-@ if none
  , _model        :: Text       -- ^ either CM or HMM
  , _modelCoord   :: FromTo     -- ^ start+end of alignment with respect to the model
  , _targetCoord  :: FromTo     -- ^ start+end of alignment with respect to the sequence
  , _strand       :: Strand     -- ^ @top/+/Watson@ strand or @bottom/-/Crick@ strand
  , _truncated    :: Truncated  -- ^ Indicates wether the hit is truncated
  , _pass         :: Text       -- ^ (debug) which pass of the pipeline the hit was detected on
  , _gc           :: Double     -- ^ G/C content fraction
  , _bias         :: Double     -- ^ ???
  , _score        :: Bitscore   -- ^ bit score, including biased composition correction
  , _evalue       :: Evalue     -- ^ e-value for hit (expected number of false positives for single query)
  , _included     :: Bool       -- ^ above inclusion threshold
  , _alignment    :: Alignment  -- ^ alignment of sequence to model (empty, is tabular alignment)
  }
  deriving (Eq,Show,Generic)

makeLenses ''Hit

