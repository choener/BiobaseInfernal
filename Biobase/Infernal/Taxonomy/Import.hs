
module Biobase.Infernal.Taxonomy.Import
  ( eeImport
  , iSpeciesMap
  , iTaxIdMap
  ) where

import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec as A
import Control.Applicative
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec.Enumerator as EAP
import qualified Data.Map as M

import Biobase.Infernal.Taxonomy

import qualified Data.Enumerator.Binary as EB



-- | Provide name-based lookup as the most-common usage scenario.
--
-- TODO there are 9 duplicates in the names, let's find them and see what is
-- going on

iSpeciesMap :: Monad m => E.Iteratee Species m (M.Map BS.ByteString Species)
iSpeciesMap = EL.fold (\m x -> M.insert (name x) x m) M.empty

-- | And a map based on taxon id

iTaxIdMap :: Monad m => E.Iteratee Species m (M.Map Int Species)
iTaxIdMap = EL.fold (\m x -> M.insert (taxid x) x m) M.empty

-- | Imports taxonomy data.
--
-- NOTE The taxonomy format is, for each species, a line consisting of: taxid -
-- tab - species name - tab - semicolon separated list of classification names
-- - dot - end of line.

eeImport :: Monad m => E.Enumeratee BS.ByteString Species m b
eeImport = E.sequence $ EAP.iterParser mkSpecies

-- | Given a 'ByteString', create a species entry.

mkSpecies :: A.Parser Species
mkSpecies = f <$> ptaxid <* tab <*> pname <* tab <*> A8.takeWhile (/='\n') <* A8.endOfLine where
  f k n xs = let
               cs = map (BS.copy . BS.dropWhile (==' ')) . BS.split ';' . BS.init $ xs
             in Species (BS.copy n) cs k
  ptaxid   = A8.decimal
  pname    = A8.takeWhile (/='\t')
  tab      = A8.char '\t'



-- * Testing

test :: IO ()
test = do
  m1 <- E.run_ $ (EB.enumFile "./Tests/Infernal/taxonomy" E.$$ (eeImport E.=$ iSpeciesMap))
  m2 <- E.run_ $ (EB.enumFile "./Tests/Infernal/taxonomy" E.$$ (eeImport E.=$ iTaxIdMap))
  print $ M.size m1
  print $ m1 M.! BS.pack "Cenarchaeum symbiosum B"
  -- print $ M.size $ M.mapKeys shortenName m1
  -- print $ M.size m2
  return ()
