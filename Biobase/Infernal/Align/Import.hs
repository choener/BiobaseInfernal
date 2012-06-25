{-# LANGUAGE OverloadedStrings #-}

-- | Parses "cmalign" results.
--
-- NOTE have not tested if this works with multiple results in a file, but
-- could ;-)

module Biobase.Infernal.Align.Import where

import Data.Iteratee as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
--import Data.Iteratee.ZLib as IZ
import Data.ByteString.Char8 as BS
import Prelude as P

import Biobase.Infernal.Align
import Biobase.Infernal.Types



-- | Transforms bytestring to list of 'Align' data.

eneeAlign :: (Monad m) => Enumeratee ByteString [Align] m a
eneeAlign = enumLinesBS ><> convStream go where
  go = do
    -- lets start with some comment lines
    cs <- I.takeWhile (("#" ==) . BS.take 1)
    -- there should be score lines now
    ss <- I.takeWhile (\s -> "# STOCKHOLM 1.0" /= s && (not $ BS.null s))
    -- Stockholm lines
    xs <- I.takeWhile (/="//")
    x <- I.head
    return [Align
      { modelIdentification = ModelIdentification ""
      , sequenceScores = P.map mkScore ss
      , stockholmAlignment = BS.unlines $ xs++[x]
      }]

-- | Creates the required sequence score.

mkScore s = SequenceScore
  { sequenceName = undefined $ ws!!0
  , sLength = read . BS.unpack $ ws!!1
  , totalBitScore = BitScore . read . BS.unpack $ ws!!2
  , structureBitScore = BitScore . read . BS.unpack $ ws!!3
  , avgProbability = read . BS.unpack $ ws!!4
  } where ws = BS.words s

-- | Convenience function creating all maps.

fromFileZip :: FilePath -> IO [Align]
fromFileZip fp = run =<< ( enumFile 8192 fp
                         . joinI
                         . enumInflate GZipOrZlib defaultDecompressParams
                         . joinI
                         . eneeAlign
                         $ stream2stream
                         )

-- | Convenience function creating all maps.

fromFile :: FilePath -> IO [Align]
fromFile fp = run =<< ( enumFile 8192 fp
                      . joinI
                      . eneeAlign
                      $ stream2stream
                      )

