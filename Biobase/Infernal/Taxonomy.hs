{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- | Infernal contains a taxonomy database. This is a simple module reflecting
-- said database.

module Biobase.Infernal.Taxonomy where

import qualified Data.ByteString.Char8 as BS
import Data.Char (toLower)
import Control.Lens

import Biobase.Infernal.Types



-- | For each species, we store the name and a classification list from most
-- general (head) to most specific (last). The database comes with the NCBI
-- taxon identifier (taxid).

data Taxonomy = Taxonomy
  { _accession      :: !(Accession Species)
  , _name           :: !(Identification Species)
  , _classification :: [Classification]
  } deriving (Show)

makeLenses ''Taxonomy

-- | Given a name such as "Drosophila Melanogaster", returns "d.melanogaster".

shortenName :: Identification Species -> Identification Species
shortenName (ID xs)
  | null ws   = ID xs
  | [w] <- ws = ID w
  | otherwise = ID . BS.map toLower $ BS.take 1 (ws!!0) `BS.append` (BS.cons '.' $ ws!!1)
  where ws = BS.words xs
