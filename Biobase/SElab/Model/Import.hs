
module Biobase.SElab.Model.Import where

import           Control.Lens (set)
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
attachHMMs = CL.head >>= go
  where go Nothing = return ()
        go (Just (Right cm)) = do
          mhmm <- CL.peek
          case mhmm of
            Nothing -> yield cm >> return ()
            Just (Left h) -> do
              CL.drop 1
--              yield $ set hmm h cm
              CL.head >>= go
            Just (Right cm') -> do
              yield cm
              go (Just (Right cm'))

