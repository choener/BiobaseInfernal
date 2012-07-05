
-- | Low-level covariance models suitable for high-performance computation.

module Biobase.Infernal.CM.LowLevel where

import Data.PrimitiveArray
import Data.PrimitiveArray.Unboxed.Zero

import Biobase.Infernal.CM



-- | High-performance CM data structures. We use 'PrimitiveArray's and
-- flattened lookups.

data CMll = CMll
  {
  }

-- | Transform CM to low-level CM.

fromCM :: CM -> CMll
fromCM = undefined
