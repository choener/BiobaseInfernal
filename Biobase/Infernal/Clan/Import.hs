{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Importing clan data is probably never time-critical as the total file size
-- is extremely small. Should this ever change, swap to iteratee. The file is
-- read /strictly/.

module Biobase.Infernal.Clan.Import where

import qualified Data.ByteString.Char8 as BS
import Data.List

import Biobase.Infernal.Clan
import Biobase.Infernal.Types



-- | Import the complete data from an uncompressed source file.

fromFile :: FilePath -> IO [Clan]
fromFile fp = fromByteString `fmap` BS.readFile fp

-- | Transform a bytestring into a list of 'Clan's.

fromByteString :: BS.ByteString -> [Clan]
fromByteString s = map mkClan
                 . groupBy (\x y -> "AC"/=(head . BS.words $y))
                 . BS.lines
                 $ s

-- | Given a list of bytestrings, create one Clan.
--
-- TODO return Maybe, make crash-safe (not really high on the list...)

mkClan :: [BS.ByteString] -> Clan
mkClan xs = Clan
  { cAccession  = ClanAccession . f . BS.drop 2 . (!!1) . BS.words . head . filter ((=="AC") . BS.take 2) $ xs
  , cIdentifier = ClanIdentification . (!!1) . BS.words . head . filter ((=="ID") . BS.take 2) $ xs
  , cMembers    = map (ModelAccession . f . BS.drop 2 . BS.init . (!!1)) . filter ((=="MB") . (!!0)) . map BS.words $ xs
  , cStrings    = xs
  } where
      f s
        | Just (k, _) <- BS.readInt s = k
        | otherwise = error $ "mkClan: " ++ BS.unpack s
