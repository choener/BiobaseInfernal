{-# LANGUAGE OverloadedStrings #-}

-- | Rfam clans are a set of biologically related Rfam families. This module
-- provides simple abstraction methods and loaders from file and ByteString.
--
-- TODO load and parse with enumerator

module Biobase.Infernal.Clan where

import qualified Data.ByteString.Char8 as BS



-- | Simple Rfam clan data.

data Clan = Clan
  -- | result of the "AC    CL00001" line, keeping "1" in this case.
  { accession  :: Int -- BS.ByteString
  -- | the "ID    tRNA" line, keeping "tRNA".
  , identifier :: BS.ByteString
  -- | all the "MB    RF00005;", "MB    RF00023;" lines, keeping "[5,23]".
  , members    :: [Int] -- [BS.ByteString]
  -- | all lines of each clan, without any processing (except being in lines).
  , strings    :: [BS.ByteString]
  } deriving (Read,Show,Eq)

