{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

-- | Imports an Rfam Fasta file and provides simultaneous export to four
-- different data structures for lookups.

module Biobase.Infernal.RfamFasta.Import where

import Control.Arrow ((***))
import Data.ByteString.Char8 as BS
import Data.Iteratee as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
--import Data.Iteratee.ZLib as IZ
import Data.Map as M
import Prelude as P

import Biobase.Infernal.RfamFasta
import Biobase.Infernal.Types



-- | Enumeratee for RfamFasta entries from a ByteString.

eneeRfamFasta :: (Monad m) => Enumeratee ByteString [RfamFasta] m a
eneeRfamFasta = enumLinesBS ><> convStream f where
  f = do
    th <- I.tryHead
    case th of
      Nothing -> error "huh?"
      Just h  -> do
                   let (ana,sps) = (BS.split ';' *** BS.split ':' . BS.dropWhile (==' ')) . BS.break (==' ') $ h
                   fs <- I.takeWhile (\s -> ">" /= BS.take 1 s)
                   return . (:[]) $ RfamFasta
                     { modelAccession    = ModelAC . read . P.drop 2 . unpack $ ana!!0
                     , modelIdentifier   = ModelID $ ana!!1
                     , sequenceAccession = mkEmblAC $ ana!!2
                     -- , speciesAC = maybe (error $ "ERROR: " ++ show (unpack $ sps!!0,unpack s)) fst . readInt $ sps!!0
                     , speciesAccession  = SpeciesAC . maybe (-1) fst . readInt $ sps!!0
                     , speciesName = SpeciesName $ sps!!1
                     , fastaData = StrictSeqData . BS.copy . BS.concat $ fs
                     }



-- * In-memory lookup

-- | Create a mapping between rfam family accession numbers and rfam family
-- names.

iModelAC2ID :: (Monad m) => Iteratee [RfamFasta] m ModelAC2ID
iModelAC2ID = I.foldl' f M.empty where
  f !m x = insertWith' const (modelAccession x) (modelIdentifier x) m

-- | Create a mapping between rfam family names and rfam family accession
-- numbers.

iModelID2AC :: (Monad m) => Iteratee [RfamFasta] m ModelID2AC
iModelID2AC = I.foldl' f M.empty where
  f !m x = insertWith' const (modelIdentifier x) (modelAccession x) m

-- | Provides a mapping between (Rfam accession, sequence accession) and the
-- complete 'RfamFasta'.

iACAC2RfamFasta :: (Monad m) => Iteratee [RfamFasta] m ACAC2RfamFasta
iACAC2RfamFasta = I.foldl' f M.empty where
  f !m x = insertWith' union (modelAccession x) (M.singleton (sequenceAccession x) x) m

-- | Provides a mapping between (Rfam name, sequence accession) and the complete
-- 'RfamFasta'.

iIDAC2RfamFasta :: (Monad m) => Iteratee [RfamFasta] m IDAC2RfamFasta
iIDAC2RfamFasta = I.foldl' f M.empty where
  f !m x = insertWith' union (modelIdentifier x) (M.singleton (sequenceAccession x) x) m



-- * File reading.

-- | Convenience function creating all maps.

{-
fromFileZip :: FilePath -> IO (ModelAC2ID, ModelID2AC, ACAC2RfamFasta, IDAC2RfamFasta)
fromFileZip fp = run =<< ( enumFile 8192 fp
                         . joinI
                         . enumInflate GZipOrZlib defaultDecompressParams
                         . joinI
                         . eneeRfamFasta
                         $ I.zip4 iModelAC2ID iModelID2AC iACAC2RfamFasta iIDAC2RfamFasta
                         )
-}

-- | Convenience function creating all maps.

fromFile :: FilePath -> IO (ModelAC2ID, ModelID2AC, ACAC2RfamFasta, IDAC2RfamFasta)
fromFile fp = run =<< ( enumFile 8192 fp
                      . joinI
                      . eneeRfamFasta
                      $ I.zip4 iModelAC2ID iModelID2AC iACAC2RfamFasta iIDAC2RfamFasta
                      )

