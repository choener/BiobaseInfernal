{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- | Infernal contains a taxonomy database. This is a simple module reflecting
-- said database.

module Biobase.SElab.Taxonomy where

import Biobase.Types.Taxonomy

import Biobase.SElab.Taxonomy.Import



{-

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
shortenName (IDD xs)
  | null ws   = IDD xs
  | [w] <- ws = IDD w
  | otherwise = IDD . BS.map toLower $ BS.take 1 (ws!!0) `BS.append` (BS.cons '.' $ ws!!1)
  where ws = BS.words xs
-}

