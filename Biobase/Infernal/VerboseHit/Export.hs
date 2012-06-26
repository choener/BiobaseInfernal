{-# LANGUAGE NoMonomorphismRestriction #-}
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

import Biobase.Infernal.Types
import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Transforms a list of verbose hits into a bytestring.
--
-- TODO How to append the last line "//" to the finished stream, if at least
-- one element was printed?

eneeByteString :: Monad m => Enumeratee [VerboseHit] ByteString m a
eneeByteString = eneeByteStrings ><> mapChunks BS.concat

-- | This transformer keeps a 1-1 relationship between each 'VerboseHit' and
-- bytestring representation. Useful for merging different streams, if
-- individual 'VerboseHit's are to be annotated.

eneeByteStrings :: Monad m => Enumeratee [VerboseHit] [ByteString] m a
eneeByteStrings = unfoldConvStream f (AliGo BS.empty BS.empty '?' []) where
  f acc = do
    h <- I.head
    let na = newAcc acc h
    p <- I.peek
    return ( fst na
           , return . BS.unlines $ snd na ++ P.map (append "##") (vhAnnotation h)  ++ [showVerboseHit h] ++ maybe ["//"] (const []) p
           )

-- | Given the current state "a" and verbose hit "h", determine if any state
-- switches have to be emitted.

newAcc a@(AliGo{..}) h@VerboseHit{..}
  | otherwise = ( AliGo (unModelID vhModel) (unScaffold vhTarget) vhStrand [], ls )
  where ls = [ "//" | aliCM /= BS.empty && bCM ] ++
             [ "CM: " `BS.append` unModelID vhModel | bCM ] ++
             [ ">" `BS.append` unScaffold vhTarget `BS.append` "\n" | bCM || bSc] ++
             [ str `BS.append` " strand results:\n" | bCM || bSc || bSt ]
        bCM = aliCM /= unModelID vhModel
        bSc = aliScaffold /= unScaffold vhTarget
        bSt = aliStrand /= vhStrand
        str
          | vhStrand == '+' = "Plus"
          | vhStrand == '-' = "Minus"
          | otherwise       = "Unknown"



-- | Convert a 'VerboseHit' to a string, ready for printing as in the input
-- file.

showVerboseHit :: VerboseHit -> BS.ByteString
showVerboseHit VerboseHit{..} = BS.unlines
  [ BS.pack $ printf " Query = %d - %d, Target = %d - %d"
                vhModelStart vhModelStop vhTargetStart vhTargetStop
  , BS.pack $ printf " Score = %.2f, E = %f, P = %.4e, GC = %d"
                (unBitScore vhBitScore) vhEvalue vhPvalue vhGCpercent
  , ""
  , ws11 `BS.append` vhWuss
  , (BS.pack $ printf "%10d " vhModelStart)
    `BS.append` vhConsensus
    `BS.append` (BS.pack $ printf " %d" vhModelStop)
  , ws11 `BS.append` vhScoring
  , (BS.pack $ printf "%10d " vhTargetStart)
    `BS.append` vhSequence
    `BS.append` (BS.pack $ printf " %d" vhTargetStop)
  ] where
    ws11 = BS.pack $ P.replicate 11 ' '



{-
--import Biobase.Infernal.VerboseHit.Import

test = do
  xs <- fromFile "/home/choener/tmp/infernal-1.0.2/tutorial/tmp.res"
  i <- enumList [xs] $ joinI $ eneeByteString stream2stream
  ys <- run i
  {-
  BS.putStrLn ys
  print $ BS.length ys
  print $ P.length $ BS.lines ys
  -}
  BS.putStrLn $ BS.take 1000 ys
  return ()
-}

