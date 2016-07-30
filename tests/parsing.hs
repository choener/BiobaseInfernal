
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
import           Control.Monad.Trans.Resource (runResourceT)
import           Control.Monad.Trans.Writer.Strict
import           Control.Monad (when)
import           Criterion.Main
import           Data.Conduit
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit.Zlib
import qualified Data.Text as T
import qualified Data.Text.IO as T

import           Biobase.SElab.CM.Types as CM
import           Biobase.SElab.HMM.Types as HMM
import           Biobase.SElab.Model.Import
import           Biobase.SElab.Model.Types



-- | Parse only the header

parseOnlyHeader :: String -> IO [PreModel]
parseOnlyHeader file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $$ consume
  when (not $ T.null log) $ T.putStr log
--  print $ length xs
--  print $ xs ^.. ix 0 . _Right . _1 . CM.name
--  print $ xs ^.. ix 1 . _Left . _1 . HMM.name
  return xs

parseFull :: String -> IO [CM]
parseFull file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $= finalizeModels $= attachHMMs $$ consume
  when (not $ T.null log) $ T.putStr log
  return xs

parseFullPar :: String -> IO [CM]
parseFullPar file = do
  (xs,log) <- runResourceT $ runWriterT $ sourceFile file $= ungzip $= preModels $= finalizeModels $= attachHMMs $= deepseqPar 16 $$ consume
  when (not $ T.null log) $ T.putStr log
  return xs

main :: IO ()
main = defaultMain
  -- only parse the header
  [ bgroup "header"
    [ bench "small"  $ nfIO $ parseOnlyHeader "./rfam-models/CL00001.cm.gz"
--    , bench "medium" $ nfIO (return ())
--    , bench "large"  $ nfIO (return ())
    ]
  -- full parsing, single-threaded
  , bgroup "singlethreaded"
    [ bench "small"  $ nfIO $ parseFull "./rfam-models/CL00001.cm.gz"
--    , bench "medium" $ nfIO (return ())
--    , bench "large"  $ nfIO (return ())
    ]
  -- full parsing, multi-threaded
  , bgroup "multithreaded"
    [ bench "small"  $ nfIO $ parseFullPar "./rfam-models/CL00001.cm.gz"
--    , bench "medium" $ nfIO (return ())
--    , bench "large"  $ nfIO (return ())
    ]
  ]


{-
import Numeric.Log

import Biobase.SElab.UnsafeNumeric



main = defaultMain
  -- performance of save vs. unsafe addition in log-space
  [ bgroup "+"  [ bench "Log:Num" $ nf ((+) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (add (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns:alternativeAdd" $ nf (add (Exp (1::Double))) (Exp 2)
                ]
  -- performance of save vs. unsafe multiplication in log-space
  , bgroup "*"  [ bench "Log:Num" $ nf ((*) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (mul (Exp (1::Double))) (Exp 2)
                ]
  ]
-}

