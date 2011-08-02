{-# LANGUAGE BangPatterns #-}

-- | Iteratee-based importer. Provides a simple "fromFile" function that
-- produces both maps in one pass.

module Biobase.Infernal.Taxonomy.Import where

import Control.Applicative
import Data.Attoparsec as A
import Data.Attoparsec.Char8 as A8
import Data.Attoparsec.Iteratee
import Data.ByteString.Char8 as BS
import Data.Either.Unwrap as E
import Data.Iteratee as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
import Data.Iteratee.ListLike as I
import Data.List as L
import Data.Map as M

import Biobase.Infernal.Taxonomy



-- | Provide name-based lookup as the most-common usage scenario.
--
-- TODO there are 9 duplicates in the names, let's find them and see what is
-- going on

iSpeciesMap :: Monad m => Iteratee [Species] m (M.Map ByteString Species)
iSpeciesMap = I.foldl' f M.empty where
  f !m x = M.insert (name x) x m

-- | And a map based on taxon id

iTaxIdMap :: Monad m => Iteratee [Species] m (M.Map Int Species)
iTaxIdMap = I.foldl' f M.empty where
  f !m x = M.insert (taxid x) x m

-- | Imports taxonomy data.

eneeSpecies :: Monad m => Enumeratee ByteString [Either String Species] m a
eneeSpecies = enumLinesBS ><> mapStream (parseOnly mkSpecies)

-- | Given a 'ByteString', create a species entry.
--
-- NOTE The taxonomy format is, for each species, a line consisting of: taxid -
-- tab - species name - tab - semicolon separated list of classification names
-- - dot - end of line.

mkSpecies :: Parser Species
mkSpecies = f <$> ptaxid <* tab <*> pname <* tab <*> takeByteString where
  f k n xs = let
               cs = L.map (copy . BS.dropWhile (==' ')) . BS.split ';' . BS.init $ xs
             in Species (copy n) cs k
  ptaxid   = decimal
  pname    = A8.takeWhile (/='\t')
  tab      = char '\t'

-- | Convenience function: given a taxonomy file, produce both maps simultanously.

fromFile :: FilePath -> IO (M.Map ByteString Species, M.Map Int Species)
fromFile fp = do
  i <- enumFile 8192 fp
    . joinI
    . (eneeSpecies ><> I.filter isRight ><> mapStream fromRight)
    $ I.zip iSpeciesMap iTaxIdMap
  run i

-- * Testing

{-
test :: IO ()
test = do
  (s,t) <- fromFile "/home/choener/tmp/taxonomy"
  print $ M.size s
  print $ M.size t
  return ()
-}
