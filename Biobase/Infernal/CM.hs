
-- | Infernal CMs.

module Biobase.Infernal.CM where

import Data.Vector as V
import Data.Vector.Unboxed as VU
import Data.ByteString as BS

import Data.PrimitiveArray
import Data.PrimitiveArray.Ix



-- | A datatype representing Infernal covariance models. This is a new
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
-- TODO as with other projects, we should not use Double's but "Score" and
-- "Probability" newtypes.

data CM = CM
  { name :: ByteString
  , accession :: Int
  , ga :: Double
  , tc :: Double
  , nc :: Double
  , transition :: PrimArray (Int,Int) Double
  , emission :: PrimArray (Int,Int) Double
  , paths :: V.Vector (VU.Vector Double)
  , localBegin :: VU.Vector Double
  , begins :: VU.Vector Int
  , localEnd :: VU.Vector (Double)
  , nodes :: V.Vector (VU.Vector Int)
  }
  deriving (Show)
