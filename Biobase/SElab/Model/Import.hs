
module Biobase.SElab.Model.Import where

import           Control.DeepSeq
import           Control.Lens (set,over,(^.))
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (when,replicateM)
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
import           Control.Parallel.Strategies (using,parList,rdeepseq,parMap)
import           Data.Maybe (catMaybes)

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
attachHMMs = go where
  go = do
    cm <- await
    case cm of
      Nothing -> return ()
      Just (Right cm) -> do
        mhmm <- await
        case mhmm of
          -- We have no attached HMM, and the stream is finished
          Nothing -> do
            yield cm
            lift . tell $ "CM: " <> (cm^.CM.name) <> " has no attached HMM and the stream is finished\n"
            return ()
          -- We have an attached HMM
          Just (Left h) -> do
            yield $ set hmm (over HMM.accession retagAccession h) cm
--            lift . tell $ "CM: " <> (cm^.CM.name) <> (_getAccession $ cm^.CM.accession) <> " all is fine!\n"
            go
          -- We have no attached HMM, a CM is coming in
          Just (Right cm') -> do
            yield cm
            lift . tell $ "CM: " <> (cm^.CM.name) <> " has no attached HMM and is followed by " <> (cm'^.CM.name) <> "\n"
            leftover $ Right cm'
            go

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
        -- nothing left, but we need to send the remaining text downstream
        Nothing -> do
          let yld = T.concat . L.reverse $ pre : ls
          yield yld
          return ()
        -- try splitting at model boundaries
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

finalizeModels :: (Monad m) => Int -> Conduit PreModel (WriterT Text m) Model
finalizeModels n' = go where
  n = max 1 n'
  go = do
    xs <- catMaybes <$> replicateM n await
    let ys = parMap rdeepseq parsePreModel xs
    mapM_ report ys
    if null xs
      then return ()
      else go
  parsePreModel (Left (hmm,s)) = Left  (AT.parseOnly (parseHMMBody hmm) s, hmm, s)
  parsePreModel (Right (cm,s)) = Right (AT.parseOnly (parseCMBody cm) s, cm, s)
  -- success
  report (Left  (Right p, _, _)) = yield $ Left p
  report (Right (Right p, _, _)) = yield $ Right p
  -- failure
  report (Left (Left err, hmm, s)) = do
    lift . tell $ "finalizeModels: can't finalize HMM "
                <> (hmm^.HMM.name)
                <> "\nERROR:\n"
                <> T.pack err <> "\n" <> s <> "\n"
  report (Right (Left err, cm, s)) = do
    lift . tell $ "finalizeModels: can't finalize CM "
                <> (cm^.CM.name)
                <> "\nERROR:\n" <>
                T.pack err <> "\n" <> s <> "\n"
  {-
  go = do
    x <- await
    case x of
      Nothing -> return ()
      Just (Left (hmm,s)) -> case AT.parseOnly (parseHMMBody hmm) s of
        Left err -> do
          lift . tell $ "finalizeModels: can't finalize HMM "
                      <> (hmm^.HMM.name)
                      <> "\nERROR:\n"
                      <> T.pack err <> "\n" <> s <> "\n"
          go
        Right p  -> do
          yield $ Left p
          go
      Just (Right (cm,s)) -> case AT.parseOnly (parseCMBody cm) s of
        Left err -> do
          lift . tell $ "finalizeModels: can't finalize CM "
                      <> (cm^.CM.name)
                      <> "\nERROR:\n" <>
                      T.pack err <> "\n" <> s <> "\n"
          go
        Right p  -> do
          yield $ Right p
          go
-}

-- | Evaluate in parallel

{-
deepseqPar :: forall m x . (Monad m, NFData x) => Int -> Conduit x (WriterT Text m) x
deepseqPar n = go where
  go = do
    xs <- trace "before" $ replicateM n $ do
            Just x <- await
            z <- lift $ WriterT x
            return $ Just x
    let ys :: [Maybe x] = xs `using` parList rdeepseq
    let zs :: [x] = [ y | Just y <- ys ]
    trace "after" $ zs `deepseq` mapM_ yield zs
-}

