
module Biobase.SElab.Model.Import where

import           Control.DeepSeq
import           Control.Lens (set,over,(^.))
import           Control.Monad (void,unless)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Resource (MonadThrow)
import           Control.Monad.Trans.Resource (runResourceT,MonadThrow)
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (when,replicateM)
import           Control.Parallel.Strategies (using,parList,rdeepseq,parMap)
import           Data.ByteString (ByteString)
import           Data.Conduit
import           Data.Conduit.Text (decodeUtf8)
import           Data.Conduit.Zlib
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.Text (Text)
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as BS
import qualified Data.Conduit.Combinators as CC
import qualified Data.Conduit.List as CL
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           System.FilePath (takeExtension)
import           System.IO (stdin)

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
    -- prepare the incoming bytestring to have boundaries after each "\n//"
    prepare ls pre = do
      x <- await
      case x of
        -- nothing left, but we need to send the remaining text downstream
        Nothing -> do
          let yld = T.concat . L.reverse $ pre : ls
          unless (T.null yld) $ yield yld
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
--
-- The parameter @n@ specifies the number of models to finalize in
-- parallel. Unless memory is extremely tight, a value of @32-64@ should
-- give nice speedups on typical machines. We use @parMap rdeepseq@.

finalizeModels
  :: (Monad m)
  => Int
  -- ^ models to parse in parallel.
  -> Conduit PreModel (WriterT Text m) Model
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

-- | Load a number of models from file.
--
-- TODO We later on want @(Int,PreModel) -> Bool@ as a filter, where the
-- @Int@ is the running number of models seen. Models with same ACC get the
-- same id.

fromFile
  :: FilePath
    -- ^ input file name. Can be @-@ for stdin. If a file and the file ends
    -- with @.gz@, the file is uncompressed on the ly.
  -> Int
    -- ^ amount of parallelism when parsing models (@16-64@ seems useful)
  -> ((Int,PreModel) -> Bool)
    -- ^ filter premodels before they are fully parsed. Full parsing is
    -- costly. Use @const True@ if unsure.
  -> IO ([CM], Text)
fromFile fp np preFilter
  | fp == "-"                 = parse (CC.sourceHandle stdin)
  | takeExtension fp == ".gz" = parse (CC.sourceFile fp $= ungzip)
  | otherwise                 = parse (CC.sourceFile fp)
  where
    parse source =
      runResourceT $ runWriterT $ source
      $= preModels
      $= (void $ CL.mapAccum accIndex (M.empty))
      $= CL.filter preFilter
      $= CL.map snd
      $= finalizeModels np $= attachHMMs
      $$ CL.consume
    accIndex
      :: PreModel -- the incoming premodel
      -> M.Map (Accession ()) Int -- a map of known accession/running index values
      -> (M.Map (Accession ()) Int, (Int,PreModel)) -- updated map, combined index+premodel
    -- During return, we always @insert@, as we assume that running indices
    -- are typically new. Every second index should be new, unless there
    -- are duplicate entries for an accession, then there is more than just
    -- one CM and one HMM.
    accIndex (Left (hmm,thmm)) ai =
      let acc = retagAccession $ hmm^.HMM.accession
          ix  = M.findWithDefault (M.size ai + 1) acc ai
      in  (M.insert acc ix ai, (ix, Left (hmm,thmm)))
    accIndex (Right (cm,tcm)) ai =
      let acc = retagAccession $ cm ^.CM.accession
          ix  = M.findWithDefault (M.size ai + 1) acc ai
      in  (M.insert acc ix ai, (ix, Right (cm,tcm)))

