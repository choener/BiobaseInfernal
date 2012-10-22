{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Import HMMER3 HMM models.

module Biobase.Hmmer.HMM.Import where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Double as BS
import Data.Conduit as C
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Control.Monad (unless)
import Prelude as P

import Biobase.Hmmer.HMM



parseHMM3 :: (Monad m, MonadIO m) => Conduit ByteString m HMM3
parseHMM3 = CB.lines =$= CL.sequence go where
  go = do
    hdr <- CL.head
    unless (legalHMM hdr) . error $ "no legal HMM at header: " ++ show hdr
    hs <- headerLines
    liftIO $ print hs
    killLines
    return $ HMM3
      {
      }

headerLines = go [] where
  go xs = do
    p <- CL.peek
    case p of
      (Just x) | "HMM" `BS.isPrefixOf` x -> return $ P.reverse xs
               | otherwise -> CL.drop 1 >>  go (x:xs)
      Nothing -> error $ "no more lines after: " ++ show (P.reverse xs)

killLines = do
  h <- CL.head
  case h of
    (Just "//") -> return ()
    _           -> killLines

legalHMM :: Maybe ByteString -> Bool
legalHMM (Just s)
  | s == "HMMER3/f [i1.1rc1 | June 2012]" = True
  | s == "HMMER3/b [3.0b2 | June 2009]"   = True
legalHMM _ = False



test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "test.hmm" $= parseHMM3 $$ consume -- sinkHandle stdout
  print xs

