
module Biobase.SElab.Model.Import where

import           Control.Lens (set,over,(^.))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.Strict
import           Data.Conduit
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Conduit.List as CL
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import           Data.Conduit.Combinators

import           Biobase.Types.Accession

import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Types



type PreModel = Either (HMM (), ByteString) (CM, ByteString)

type Logger m = WriterT ErrorLog m

newtype ErrorLog = Seq Text

-- | Combine CMs with their HMMs. Assumes that each CM is followed by its
-- HMM.
--
-- The @WriterT@ logs errors during HMM/CM combination.
--
-- TODO use leftover instead of peeking?

attachHMMs :: (Monad m) => Conduit Model (WriterT Text m) CM
attachHMMs = CL.head >>= go
  where go Nothing = return ()
        go (Just (Right cm)) = do
          mhmm <- CL.peek
          case mhmm of
            -- We have no attached HMM, and the stream is finished
            Nothing -> do
              yield cm
              lift . tell $ "CM: " <> (cm^.CM.name) <> " has no attached HMM!"
            -- We have an attached HMM
            Just (Left h) -> do
              CL.drop 1
              yield $ set hmm (over HMM.accession retagAccession h) cm
              lift . tell $ "CM: " <> (cm^.CM.name) <> (_getAccession $ cm^.CM.accession) <> " all is fine!"
            -- We have no attached HMM, a CM is coming in
            Just (Right cm') -> do
              yield cm
              lift . tell $ "CM: " <> (cm^.CM.name) <> " has no attached HMM!"
          CL.head >>= go
        go (Just (Left hmm)) = do
          lift . tell $ "HMM: " <> (hmm^.HMM.name) <> " has no CM it belong to and is being dropped!"
          CL.head >>= go

-- | High-level parsing of stochastic model files. For each model, the
-- header is extracted, together with a bytestring with the remainder that
-- needs to be parsed yet.

preModels :: (Monad m) => Conduit ByteString (WriterT Text m) PreModel
preModels = linesUnboundedAscii =$= go
  where go = do
          CL.dropWhile (not . header)
          ls <- CL.takeWhile notSlashed
          -- TODO call appropriate parser
          -- TODO yield premodel
          go
        header l = "INFERNAL" `T.isPrefixOf` l || "HMMER" `T.isPrefixOf` l

-- | Complete the parsing procedure turning a premodel into a model.

finalizeModels :: () => Conduit PreModel (WriterT Text m) Model
finalizeModels = undefined

