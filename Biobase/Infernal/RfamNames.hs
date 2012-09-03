{-# LANGUAGE TemplateHaskell #-}

-- | The database of Rfam "names". For each model, we get to know which
-- sequences it is built of, what the AC of the species is, and its name (or
-- ID).

module Biobase.Infernal.RfamNames where

import Control.Lens

import Biobase.Infernal.Types


data ModelNames = ModelNames
  { _modelAC     :: !(Accession Rfam)
  , _modelID     :: !(Identification Rfam)
  -- TODO this would have been the sequence info
  , _speciesAC   :: Maybe (Accession Species)
  , _speciesID   :: Maybe (Identification Species)
  } deriving (Show)

makeLenses ''ModelNames
