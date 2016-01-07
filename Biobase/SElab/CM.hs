
-- | Infernal CMs.
--
-- TODO order of nucleotides? ACGU?
--
-- TODO "fastCM :: CM -> FastCM" to make a data structure that is suitable for
-- high-performance applications.

module Biobase.SElab.CM
  ( module Biobase.SElab.CM.Types
  , module Biobase.SElab.CM.Import
  ) where

import Biobase.SElab.CM.Import (conduitCM,fromFile)
import Biobase.SElab.CM.Types

