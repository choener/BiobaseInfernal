{-# LANGUAGE OverloadedStrings #-}

-- | Rfam clans are a set of biologically related Rfam families. This module
-- provides simple abstraction methods and loaders from file and ByteString.
--
-- TODO load and parse with enumerator

module Biobase.Infernal.Clan where

import Data.ByteString.Char8 (ByteString)

import Biobase.Infernal.Types



-- | Simple Rfam clan data.

data Clan = Clan
  -- | result of the "AC    CL00001" line, keeping "1" in this case.
  { cAccession  :: !ClanAccession
  -- | the "ID    tRNA" line, keeping "tRNA".
  , cIdentifier :: !ClanIdentification
  -- | all the "MB    RF00005;", "MB    RF00023;" lines, keeping "[5,23]".
  , cMembers    :: ![ModelAccession]
  -- | all lines of each clan, without any processing (except being in lines).
  , cStrings    :: ![ByteString]
  } deriving (Read,Show,Eq)

