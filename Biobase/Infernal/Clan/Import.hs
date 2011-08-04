{-# LANGUAGE OverloadedStrings #-}

-- | Importing clan data is probably never time-critical as the total file size
-- is extremely small. Should this ever change, swap to iteratee. The file is
-- read /strictly/.

module Biobase.Infernal.Clan.Import where

import qualified Data.ByteString.Char8 as BS
import Data.List

import Biobase.Infernal.Clan



-- | Import the complete data strictly.

fromFile :: FilePath -> IO [Clan]
fromFile fp = do
  (map mkClan . groupBy (\x y -> "AC"/=(head . BS.words $y)) . BS.lines) `fmap` BS.readFile fp

-- | Given a list of bytestrings, create one Clan.
--
-- TODO return Maybe, make crash-safe

mkClan :: [BS.ByteString] -> Clan
mkClan xs = Clan
  { accession  = (!!1) . BS.words . head . filter ((=="AC") . BS.take 2) $ xs
  , identifier = (!!1) . BS.words . head . filter ((=="ID") . BS.take 2) $ xs
  , members    = map BS.init . map (!!1) . filter ((=="MB") . (!!0)) . map BS.words $ xs
  , strings    = xs
  }
