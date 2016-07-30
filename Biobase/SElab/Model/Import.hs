
module Biobase.SElab.Model.Import where

import           Control.Lens (set,over,(^.))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Trans.Writer.Strict
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Conduit.Text (decodeUtf8)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text (Text)
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Debug.Trace

import           Biobase.Types.Accession

import           Biobase.SElab.CM.Import as CM
import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Import as HMM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Types



type PreModel = Either (HMM (), Text) (CM, Text)

type Logger m = WriterT ErrorLog m

newtype ErrorLog = Seq Text

-- | Combine CMs with their HMMs. Assumes that each CM is followed by its
-- HMM.
--
-- The @WriterT@ logs errors during HMM/CM combination.
--
-- TODO use leftover instead of peeking?
--
-- TODO got the order wrong ;-)

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

preModels :: (Monad m, MonadThrow m) => Conduit ByteString (WriterT Text m) PreModel
preModels = decodeUtf8 =$= prepare [] T.empty =$= premodel
  where
--    header l = "INFERNAL" `T.isPrefixOf` l || "HMMER" `T.isPrefixOf` l
--    slashed = T.isPrefixOf "\\"
    -- prepare the incoming bytestring to have boundaries after each "\n//"
    prepare ls pre = do
      x <- await
      case x of
        Nothing -> return ()
        Just l' -> do
          let l = pre <> l'
          let (p,q) = T.breakOn "\n//" l
          if "\n//" `T.isPrefixOf` q
            then do let yld = T.concat . L.reverse $ "\n//" : p : ls
                    yield yld
                    prepare [] (T.stripStart $ T.drop 3 q)
            else prepare (pre:ls) l'
    premodel = do
      s <- await
      case s of
        Nothing -> return ()
        Just t -> if
          | "INFERNAL" `T.isPrefixOf` t -> case AT.parseOnly parsePreCM t of
            Left err -> do lift . tell $ "preModels: " <> T.pack err
                           premodel
            Right p -> do yield $ Right p
                          premodel
          | "HMMER"    `T.isPrefixOf` t -> case AT.parseOnly parsePreHMM t of
            Left err -> do lift . tell $ "preModels: " <> T.pack err
                           premodel
            Right p -> do yield $ Left p
                          premodel
          | otherwise -> do
              lift . tell $ "preModels: unknown model beginning with:\n"
              lift . tell $ t
              premodel

-- | Complete the parsing procedure turning a premodel into a model.

finalizeModels :: (Monad m) => Conduit PreModel (WriterT Text m) Model
finalizeModels = go where
  go = do
    x <- await
    case x of
      Nothing -> return ()
      Just (Left (hmm,s)) -> case AT.parseOnly (parseHMMBody hmm) s of
        Left err -> do lift . tell $ "finalizeModels: can't finalize HMM "
                                      <> (hmm^.HMM.name)
                                      <> "\nERROR:\n"
                                      <> T.pack err <> "\n" <> s <> "\n"
        Right p  -> do yield $ Left p
      Just (Right (cm,s)) -> case AT.parseOnly (parseCMBody cm) s of
        Left err -> do lift . tell $ "finalizeModels: can't finalize CM "
                                      <> (cm^.CM.name)
                                      <> "\nERROR:\n" <>
                                      T.pack err <> "\n" <> s <> "\n"
        Right p  -> do yield $ Right p

