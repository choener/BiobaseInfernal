
module Biobase.SElab.Model.Import where

import           Control.Applicative
import           Control.DeepSeq
import           Control.Lens (set,over,(^.),zoom)
import           Control.Monad (liftM2)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (void,unless)
import           Control.Monad (when,replicateM)
import           Control.Parallel.Strategies (using,parList,rdeepseq,parMap)
import           Data.ByteString (ByteString)
import           Data.Maybe (catMaybes)
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text (Text)
import           Data.Text (Text)
import           Debug.Trace
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Pipes.Attoparsec as PA
import qualified Pipes.Parse as PP
import           System.FilePath (takeExtension)
import           System.IO (stdin,withFile,IOMode(..))
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Pipes.ByteString as PB
import qualified Pipes.GZip as PG
import qualified Pipes.Safe.Prelude as PSP

import           Biobase.Types.Accession
import           Pipes.Split.ByteString

import           Biobase.SElab.CM.Import as CM
import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Import as HMM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Types



type PreModel = Either (HMM (), ByteString) (CM, ByteString)

type Logger m = WriterT Log m

-- TODO use a builder?

newtype Log = Log { getLog :: Text }
  deriving (Monoid,IsString)

-- | Combine CMs with their HMMs. Assumes that each CM is followed by its
-- HMM.

attachHMMs :: (Monad m) => PP.Producer Model (Logger m) r -> PP.Producer CM (Logger m) ((), PP.Producer Model (Logger m) r)
attachHMMs = PP.parsed go where
  go :: (Monad m) => PP.Parser Model (Logger m) (Either () CM)
  go = do
    mcm <- PP.draw
    case mcm of
      Nothing -> return $ Left ()
      Just (Left hmm) -> do
        lift . tell . Log $ "HMM: " <> (hmm^.HMM.name) <> " is an orphan\n"
        go
      Just (Right cm) -> do
        mhm <- PP.draw
        case mhm of
          Nothing -> do
            lift . tell . Log $ "CM: " <> (cm^.CM.name) <> " has no attached HMM and the stream is finished\n"
            return $ Right cm
          -- TODO actually check that these belong together
          Just (Left hm) | (cm^.CM.name) == (hm^.HMM.name) -> do
            -- cm and hmm belong together
            return . Right $ set hmm (over HMM.accession retagAccession hm) cm
          -- The HMM doesn't belong to our CM
          Just (Left hm) -> do
            lift . tell . Log $ "CM: " <> (cm^.CM.name) <> " and HMM: " <> (hm^.HMM.name) <> " do not belong together, dropping the HMM from the stream\n"
            return . Right $ cm
          Just (Right dup) -> do
            lift . tell . Log $ "CM: " <> (cm^.CM.name) <> " has no attached HMM\n"
            PP.unDraw $ Right dup
            go

type FilterFun = Text -> Accession () -> Bool

-- | Parses @HMM@ and @CM@ models from Rfam. The filtering function takes
-- the model name and accession and allows for premature termination of the
-- parsing of the current model.

parseSelectively :: (Monad m)
  => FilterFun
  -- ^ filter function
  -> PP.Producer ByteString (Logger m) r
--  -> PP.Producer (Maybe (Either (HMM xfam) CM)) (Logger m) (r, PP.Producer ByteString (Logger m) r)
  -> PP.Producer Model (Logger m) (r, PP.Producer ByteString (Logger m) r)
parseSelectively fltr p = PP.parsed go p P.>-> P.concat where
  -- | Parse either a CM or a HMM ...
  go = do
    p <- zoom (splitKeepEnd "//") parseCM
    case p of
      Nothing -> return $ Left $ error "done"
      Just _ -> return $ Right $ (Nothing :: Maybe (Either (HMM ()) CM))
  parseCM :: Monad m => PP.StateT (PP.Producer ByteString m x) m (Maybe (Either (HMM ()) CM))
  parseCM = do
    pre <- traceShow "hi" . PA.parse $ (Left <$> parsePreHMM) <|> (Right <$> parsePreCM)
    case pre of
      Nothing -> return $ error "nothing"
      Just (Left err) -> return $ error "just left"
      Just (Right mdl) -> trace "if" $ if fltr (mdl^.modelName) (mdl^.modelAccession)
        then do
          m <- case mdl of
            Left hmm -> do
              h <- PA.parse $ parseHMMBody hmm
              return $ Left h
            Right cm -> do
              c <- trace "ho" . PA.parse $ parseCMBody cm
              return $ Right c
          case m of
            Left _ -> error "left m"
            Right x -> error $ _ x
        else PP.skipAll >> return Nothing


fromFile
  :: FilePath
    -- ^ input file name. Can be @-@ for stdin. If a file and the file ends
    -- with @.gz@, the file is uncompressed on the ly.
  -> FilterFun
    -- ^ filter premodels before they are fully parsed. Full parsing is
    -- costly. Use @\name acc -> True@ if all models should be loaded.
  -> IO [CM]
fromFile fp fltr
  | fp == "-"                 = parse (PB.fromHandle stdin)
  | takeExtension fp == ".gz" = withFile fp ReadMode $ \hdl -> parse (PG.decompress $ PB.fromHandle hdl)
  | otherwise                 = withFile fp ReadMode $ \hdl -> parse (PB.fromHandle hdl)
  where
    parse source = do
      ((xs,((),rmdr)),log) <- runWriterT . P.toListM' $ attachHMMs $ parseSelectively fltr source
      -- TODO log should be empty
      -- TODO rmdr should be empty
      return xs

-- | Load a number of models from file.
--
-- TODO We later on want @(Int,PreModel) -> Bool@ as a filter, where the
-- @Int@ is the running number of models seen. Models with same ACC get the
-- same id.

{-

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

-}

