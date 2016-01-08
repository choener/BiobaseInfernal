{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}

-- | Infernal contains a taxonomy database. This is a simple module reflecting
-- said database.
--
-- See Task # 1effbfec-7f3b-4530-a8a7-b82fef1a0c12

module Biobase.SElab.Taxonomy
  ( module Biobase.Types.Taxonomy
  , module Biobase.SElab.Taxonomy.Import
  ) where

import Biobase.Types.Taxonomy

import Biobase.SElab.Taxonomy.Import



{-
-- | Given a name such as "Drosophila Melanogaster", returns "d.melanogaster".

shortenName :: Identification Species -> Identification Species
shortenName (IDD xs)
  | null ws   = IDD xs
  | [w] <- ws = IDD w
  | otherwise = IDD . BS.map toLower $ BS.take 1 (ws!!0) `BS.append` (BS.cons '.' $ ws!!1)
  where ws = BS.words xs
-}

