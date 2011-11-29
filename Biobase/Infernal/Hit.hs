
-- | Accessors for Infernal hits.

module Biobase.Infernal.Hit where

import Data.ByteString.Char8 (ByteString)



-- | Generalized accessors for VerboseHit's and TabularHit's.

class Hit a where
  -- | Model name (like 5S_rRNA).
  model       :: a -> ByteString
  -- | Target name, typically the scaffold or chromosome where the hit occurs.
  target      :: a -> ByteString
  -- | Start of submodel.
  modelStart  :: a -> Int
  -- | Stop of submodel.
  modelStop   :: a -> Int
  -- | Start of substring in target.
  targetStart :: a -> Int
  -- | Stop of substring in target.
  targetStop  :: a -> Int
  -- | Bit score of the hit of model in target.
  bitScore    :: a -> Double
  -- | Evalue, expectation of bit score of higher in target sequence of length.
  evalue      :: a -> Double
  -- | G/C content in target.
  gcPercent   :: a -> Int
