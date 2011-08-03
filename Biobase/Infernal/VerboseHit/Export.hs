{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exports VerboseHit results back into text. As a likely scenario is a
-- pipeline where hits are to be filtered out, this provides enumeratee's that
-- handle additional annotations as required by the file format for CMs,
-- scaffolds, and strand information. If you just need a way to show the data,
-- use printVerboseHit.

module Biobase.Infernal.VerboseHit.Export where

import Control.Monad.Trans.Class (lift)
import Data.ByteString.Char8 as BS
import Data.Iteratee as I
import Data.Maybe
import Prelude as P
import Text.Printf

import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Transforms a list of verbose hits into a bytestring.
--
-- TOOD How to append the last line "//" to the finished stream, if at least
-- one element was printed?

eneeByteString :: Monad m => Enumeratee [VerboseHit] BS.ByteString m a
eneeByteString = eneeByteStrings ><> mapChunks BS.concat

-- | This transformer keeps a 1-1 relationship between each 'VerboseHit' and
-- bytestring representation. Useful for merging different streams, if
-- individual 'VerboseHit's are to be annotated.

eneeByteStrings :: Monad m => Enumeratee [VerboseHit] [ByteString] m a
eneeByteStrings = unfoldConvStream f (AliGo BS.empty BS.empty '?') where
  f acc = do
    h <- I.head
    let na = newAcc acc h
    return (fst na , return . BS.unlines $ snd na ++ [showVerboseHit h])

-- | Given the current state "a" and verbose hit "h", determine if any state
-- switches have to be emitted.

newAcc a@(AliGo{..}) h@VerboseHit{..}
  | otherwise = ( AliGo vhCM vhScaffold vhStrand, ls )
  where ls = [ "//" | aliCM /= BS.empty && aliCM /= vhCM ] ++
             [ "CM: " `BS.append` vhCM | aliCM /= vhCM ] ++
             [ ">" `BS.append` vhScaffold `BS.append` "\n" | aliScaffold /= vhScaffold ] ++
             [ str `BS.append` " strand results:\n" | aliStrand /= vhStrand ]
        str
          | vhStrand == '+' = "Plus"
          | vhStrand == '-' = "Minus"
          | otherwise       = "Unknown"



-- | Convert a 'VerboseHit' to a string, ready for printing as in the input
-- file.

showVerboseHit :: VerboseHit -> BS.ByteString
showVerboseHit VerboseHit{..} = BS.unlines
  [ BS.pack $ printf " Query = %d - %d, Target = %d - %d"
                (fst vhQuery) (snd vhQuery) (fst vhTarget) (snd vhTarget)
  , BS.pack $ printf " Score = %.2f, E = %f, P = %.4e, GC = %d"
                vhScore vhEvalue vhPvalue vhGC
  , ""
  , ws11 `BS.append` vhWuss
  , (BS.pack $ printf "%10d " (fst vhQuery))
    `BS.append` vhConsensus
    `BS.append` (BS.pack $ printf " %d" (snd vhQuery))
  , ws11 `BS.append` vhScoring
  , (BS.pack $ printf "%10d " (fst vhTarget))
    `BS.append` vhSequence
    `BS.append` (BS.pack $ printf " %d" (snd vhTarget))
  ] where
    ws11 = BS.pack $ P.replicate 11 ' '



{-
import Biobase.Infernal.VerboseHit.Import

test = do
  xs <- fromFile "/home/choener/tmp/infernal-1.0.2/tutorial/tmp.res"
  i <- enumList [xs] $ joinI $ eneeByteString stream2stream
  ys <- run i
  BS.putStrLn ys
  print $ BS.length ys
  print $ P.length $ BS.lines ys
  return ()
-}
