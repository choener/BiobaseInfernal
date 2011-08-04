{-# LANGUAGE OverloadedStrings #-}

-- | Rfam clans are a set of biologically related Rfam families. This module
-- provides simple abstraction methods and loaders.
--
-- TODO This has to go into biobase and needs to be made nice-looking.
--
-- TODO load and parse with enumerator

module Biobase.Infernal.Clan where

import qualified Data.ByteString.Char8 as BS



-- | Simple RfamClan data wrapper. Could Easily be just a list of bytestrings,
-- which it is using strings

data Clan = Clan
  { accession :: BS.ByteString
  , identifier :: BS.ByteString
  , members :: [BS.ByteString]
  , strings :: [BS.ByteString]
  } deriving (Read,Show,Eq)

