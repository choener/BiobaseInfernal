
module Biobase.SElab.Model.Import where

import           Data.Conduit
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as M

import           Biobase.SElab.CM.Types
import           Biobase.SElab.HMM.Types
import           Biobase.SElab.Model.Types



-- | Combine CMs with their HMMs. Assumes that each CM is followed by its
-- HMM.
--
-- TODO run within a logger monad

attachHMMs :: (Monad m) => Conduit (Either (HMM ()) CM) m CM
attachHMMs = undefined

