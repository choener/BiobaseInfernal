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

import Biobase.Hmmer.HMM



parseHMM3 :: (Monad m, MonadIO m) => Conduit ByteString m HMM3
parseHMM3 = CB.lines =$= CL.sequence go where
  go = do
    hdr <- CL.head
    unless (legalHMM hdr) . error $ "no legal HMM at header: " ++ show hdr
    killLines
    return $ HMM3
      {
      }

killLines = do
  h <- CL.head
  case h of
    (Just "//") -> return ()
    _           -> killLines

legalHMM :: Maybe ByteString -> Bool
legalHMM (Just "HMMER3/f [i1.1rc1 | June 2012]") = True
legalHMM _ = False
