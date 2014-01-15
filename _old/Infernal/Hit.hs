
-- | Accessors for Infernal hits.
--
-- TODO modelStartStop pair? same for target?
--
-- TODO newtypes for these returns?

module Biobase.Infernal.Hit where

import Data.ByteString.Char8 (ByteString)

import Biobase.Infernal.Types



-- | Generalized accessors for VerboseHit's and TabularHit's.

class Hit a where
  -- | Model name (like 5S_rRNA).
  model       :: a -> ModelID
  -- | Target name, typically the scaffold or chromosome where the hit occurs.
  target      :: a -> Scaffold
  -- | Start of submodel.
  modelStart  :: a -> Int
  -- | Stop of submodel.
  modelStop   :: a -> Int
  -- | Start of substring in target.
  targetStart :: a -> Int
  -- | Stop of substring in target.
  targetStop  :: a -> Int
  -- | Bit score of the hit of model in target.
  bitScore    :: a -> BitScore
  -- | Evalue, expectation of bit score of higher in target sequence of length.
  evalue      :: a -> Double
  -- | G/C content in target.
  gcPercent   :: a -> Int
