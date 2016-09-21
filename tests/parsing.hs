
-- | We benchmark different stages of parsing. @small@, @medium@, and
-- @large@ are three sets of CMs with different file sizes.
--
-- TODO
--
-- - only extract header information
-- - single-threaded full parsing
-- - multi-threaded full parsing

module Main where

import           Control.Lens
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (when)
import           Criterion.Main
import           Data.List as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Maybe (maybeToList)

import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Import
import           Biobase.SElab.Model.Types



{-

-- | Parse only the header

parseOnlyHeader :: Int -> String -> IO [PreModel]
parseOnlyHeader n file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $= ccm n $$ consume
  when (not $ T.null log) $ T.putStr log
--  print $ length xs
--  print $ xs ^.. ix 0 . _Right . _1 . CM.name
--  print $ xs ^.. ix 1 . _Left . _1 . HMM.name
  return xs

parseFull :: Int -> String -> IO [CM]
parseFull n file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $= ccm n $= finalizeModels 1 $= attachHMMs $$ consume
  when (not $ T.null log) $ T.putStr log
  return xs

parseFullPar :: Int -> String -> IO [CM]
parseFullPar n file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $= ccm n $= finalizeModels 64 $= attachHMMs $$ consume
  when (not $ T.null log) $ T.putStr log
  return xs

main :: IO ()
main = defaultMain
  -- only parse the header
  [ bgroup "header"
    [ bench "x 1"  $ nfIO $ parseOnlyHeader 1 "./rfam-models/CL00001.cm.gz"
    ]
  -- full parsing, single-threaded
  , bgroup "singlethreaded"
    [ bench "small"  $ nfIO $ parseFull   1 "./rfam-models/CL00001.cm.gz"
    , bench "medium" $ nfIO $ parseFull  10 "./rfam-models/CL00001.cm.gz"
    , bench "large"  $ nfIO $ parseFull 100 "./rfam-models/CL00001.cm.gz"
    ]
  -- full parsing, multi-threaded
  , bgroup "multithreaded"
    [ bench "small"  $ nfIO $ parseFullPar   1 "./rfam-models/CL00001.cm.gz"
    , bench "medium" $ nfIO $ parseFullPar  10 "./rfam-models/CL00001.cm.gz"
    , bench "large"  $ nfIO $ parseFullPar 100 "./rfam-models/CL00001.cm.gz"
    ]
  ]



ccm n = go where
  go = do
    xx <- await
    yy <- await
    case xx of
      Nothing -> return ()
      Just x -> do
        Prelude.mapM_ yield $ L.concat $ L.replicate n $ x : maybeToList yy
        go

-}

main :: IO ()
main = return ()

