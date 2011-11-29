{-# LANGUAGE PatternGuards #-}

-- | Infernal contains a taxonomy database. This is a simple module reflecting
-- said database.

module Biobase.Infernal.Taxonomy where

import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)



-- | For each species, we store the name and a classification list from most
-- general (head) to most specific (last). The database comes with the NCBI
-- taxon identifier (taxid).

data Species = Species
  { name           :: !BS.ByteString
  , classification :: ![BS.ByteString]
  , taxid          :: !Int
  } deriving (Show)

-- | Given a name such as "Drosophila Melanogaster", returns "d.melanogaster".

shortenName :: BS.ByteString -> BS.ByteString
shortenName xs
  | null ws   = xs
  | [w] <- ws = w
  | otherwise = BS.map toLower $ BS.take 1 (ws!!0) `BS.append` (BS.cons '.' $ ws!!1)
  where ws = BS.words xs
