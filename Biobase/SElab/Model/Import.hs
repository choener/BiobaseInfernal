
-- | Import SElab HMM and CM models, and combine these together into full
-- CM models.
--
-- TODO A pair @Right CM +++ Left HMM@ should become a @Right CM@, a lonely
-- @Left HMM@ should become a @Left HMM@, a lonely @Right CM@ should become
-- a @Right CM@.

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
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Attoparsec.Text as AT
import qualified Data.ByteString.Char8 as BS
import qualified Data.List as L
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Pipes as P
import qualified Pipes.Attoparsec as PA
import qualified Pipes.ByteString as PB
import qualified Pipes.GZip as PG
import qualified Pipes.Parse as PP
import qualified Pipes.Prelude as P
import qualified Pipes.Safe.Prelude as PSP
import           System.FilePath (takeExtension)
import           System.IO (stdin,withFile,IOMode(..))

import           Biobase.Types.Accession
import           Pipes.Split.ByteString

import           Biobase.SElab.CM.Import as CM
import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Import as HMM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Types



-- | Filter a model after the header, not the body, has been parsed.

type PreFilterFun = Text -> Accession () -> Either (HMM ()) CM -> Bool

-- | Filter a model after the full model has been parsed. This is the same
-- type as @PreFilterFun@.

type PostFilterFun = Text -> Accession () -> Either (HMM ()) CM -> Bool

-- TODO use a builder?

newtype Log = Log { getLog :: Text }
  deriving (Monoid,IsString)

-- | The type of logger we use

type Logger m = WriterT Log m


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



-- | Parses @HMM@ and @CM@ models from Rfam. The filtering function takes
-- the model name and accession and allows for premature termination of the
-- parsing of the current model.

parseSelectively :: (Monad m)
  => PreFilterFun
  -- ^ filter function for premodels
  -> PostFilterFun
  -- ^ filter function for full models
  -> PP.Producer ByteString (Logger m) r
--  -> PP.Producer (Maybe (Either (HMM xfam) CM)) (Logger m) (r, PP.Producer ByteString (Logger m) r)
  -> PP.Producer Model (Logger m) ((), PP.Producer ByteString (Logger m) r)
parseSelectively preFltr postFltr p
  = PP.parsed go p
  P.>-> P.concat
  P.>-> P.filter (\mdl -> postFltr (mdl^.modelName) (mdl^.modelAccession) mdl)
  where
  -- | Parse either a CM or a HMM ...
  go = do
    p <- zoom (splitKeepEnd "//\n") parseMdl
    -- TODO can be simplified now
    case p of
      Left () -> return $ Left ()
      Right x -> return $ Right x
  -- parse models.
  -- TODO @Either ()@ should become @Either (Maybe Error)@ and only @Left
  -- Nothing@ will be error-free stop.
  handleError err = do
    da <- PP.drawAll
    lift . tell . Log $ "could not parse:\n"
    lift . tell . Log $ error "insert err here"
    lift . tell . Log $ error "insert da here"
    lift . tell . Log $ "\n"
    return $ Left ()
  parseMdl :: Monad m => PP.StateT (PP.Producer ByteString (Logger m) x) (Logger m) (Either () (Maybe (Either (HMM ()) CM)))
  parseMdl = do
    -- if @pre@ is Nothing, the underlying producer is exhausted.
    -- if @pre@ is @Just $ Left x@, then we have a parse error.
    -- if @pre@ is @Just $ Right y@, then we have a successful parse. In
    -- this case, @y@ is either a @Left hmm@ or a @Right cm@.
    pre <- PA.parse $ (Left <$> parsePreHMM) <|> (Right <$> parsePreCM)
    case pre of
      Nothing -> handleError "premature end of parsing\n"
      Just (Left err) -> handleError err
      Just (Right mdl) -> if preFltr (mdl^.modelName) (mdl^.modelAccession) mdl
        then do
          case mdl of
            Left hmm -> do
              h <- PA.parse $ parseHMMBody hmm
              case h of
                Nothing -> handleError "premature end of parsing\n"
                Just (Left err) -> handleError err
                Just (Right hh) -> do
                  da <- PP.drawAll
                  -- TODO check if @da@ is empty?
                  return $ Right $ Just $ Left hh
            Right cm -> do
              c <- PA.parse $ parseCMBody cm
              case c of
                Nothing -> handleError "premature end of parsing\n"
                Just (Left err) -> handleError err
                Just (Right d) -> do
                  da <- PP.drawAll
                  return . Right . Just $ Right d
        else PP.skipAll >> (return . Right $ Nothing)



-- | Keep all models

keepAllModels _ _ _ = True



-- | Load a number of models from file. Including pre- and full-model
-- filtering.

fromFile
  :: PreFilterFun
    -- ^ filter premodels before they are fully parsed. Full parsing is
    -- costly. Use @\name acc hmmOrcm -> True@ if all models should be
    -- loaded.
  -> PostFilterFun
    -- ^ Filter full models before they are combined into the CM-HMM pair.
  -> Bool
    -- ^ If true, than any error during parsing means termination of the
    -- program.
  -> FilePath
    -- ^ input file name. Can be @-@ for stdin. If a file and the file ends
    -- with @.gz@, the file is uncompressed on the ly.
  -> IO [CM]
fromFile preFltr postFltr stopOnError fp
  | fp == "-"                 = parse (PB.fromHandle stdin)
  | takeExtension fp == ".gz" = withFile fp ReadMode $ \hdl -> parse (PG.decompress $ PB.fromHandle hdl)
  | otherwise                 = withFile fp ReadMode $ \hdl -> parse (PB.fromHandle hdl)
  where
    parse source = do
      ((xs,((),rmdr)),log) <- runWriterT . P.toListM' $ attachHMMs $ parseSelectively preFltr postFltr source
      -- TODO log should be empty
      -- TODO rmdr should be empty
      let Log l = log
      unless (T.null l) $ do
        T.putStrLn l
        T.putStrLn "There have been errors parsing the models!"
        when stopOnError $ do
          error "stopping here!"
      return xs

