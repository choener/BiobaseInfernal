{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Exports VerboseHit results back into text. As a likely scenario is a
-- pipeline where hits are to be filtered out, this provides enumeratee's that
-- handle additional annotations as required by the file format for CMs,
-- scaffolds, and strand information. If you just need a way to show the data,
-- use printVerboseHit.

module Biobase.Infernal.VerboseHit.Export where
{-
  ( eeFromVerboseHit
  ) where
-}

import Control.Monad.Trans.Class (lift)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL
import Text.Printf

import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Takes a list of 'VerboseHit's and produces a list of bytestrings. Unlining
-- those bytestrings produces a file that is \in essence\ an Infernal
-- verbose-hit output file and should be parse-able by ours and other
-- importers.
--
-- TODO block length (for the alignment of query/sequenc) ?!
--
-- TODO is there a more elegant treatment of the eof condition than asking at
-- every verbose hit that is created?

{-
eeFromVerboseHit :: Monad m => E.Enumeratee VerboseHit BS.ByteString m a
eeFromVerboseHit = goS (AliGo "" "" '?') where
  goS s (E.Continue k) = EL.head >>= go s where
    go s Nothing = return $ E.Continue k
    go s@AliGo{..} (Just l@VerboseHit{..}) = do
      eof <- E.isEOF
      let res =  [ "\n//\n" | vhCM /= aliCM && aliCM /= "" ]
              ++ [ "\nCM: " `BS.append` vhCM `BS.append` "\n" | vhCM /= aliCM]
              ++ [ "\n>" `BS.append` vhSeqName `BS.append` "\n" | vhSeqName /= aliScaffold]
              ++ [ strand vhStrand | vhStrand /= aliStrand]
              ++ [ BS.pack $ printf " Query = %d - %d, Target = %d - %d"
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
                 ]
              ++ [ "\n//" | eof]
      newStep <- lift $ E.runIteratee $ k $ E.Chunks res
      goS (AliGo vhCM vhSequence vhStrand) newStep
    strand '+' = "  Plus strand results:\n"
    strand '-' = "  Minus strand results:\n"
    strand _   = "  Unknown strand results:\n"
    ws11 = BS.pack $ replicate 11 ' '
  goS _ step = return step
-}

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
    ws11 = BS.pack $ replicate 11 ' '

-- | CM information, ready for printing.

showCM :: BS.ByteString -> BS.ByteString
showCM cm = "CM: " `BS.append` cm

-- | Scaffold information

showScaffold :: BS.ByteString -> BS.ByteString
showScaffold sc = ">" `BS.append` sc

-- | Strand information

showStrand :: Char -> BS.ByteString
showStrand = f where
  f '+' = "  Plus strand results:"
  f '-' = "  Minus strand results:"
  f _   = "  Unknown strand results:"

-- | Turning a list of 'VerboseHit's back into lines of characters is, in
-- principle, not too hard. But just before we actually stream out, we might
-- want to inject arbitrary data into the stream. This is done via
-- 'StreamInsertion'. The other constructors merely wrap certain data.
--
-- One way to, say, tag verbose hits is like this (note the output type of
-- 'eeHitToStream'):
--
-- > tag [s@(StreamVerboseHit _)] = [StreamInsertion (), s]
-- > tag xs = xs

data HitStream a
  = StreamVerboseHit {streamVerboseHit :: VerboseHit}
  | StreamCM {streamCM :: BS.ByteString}
  | StreamScaffold {streamScaffold :: BS.ByteString}
  | StreamStrand {streamStrand :: Char}
  | StreamInsertion {streamInsertion :: a}
  deriving (Show)

-- | This enumeratee turns 'VerboseHit's into a 'HitStream'. Each VerboseHit
-- can emit one or more elements, depending on if the CM, scaffold, or strand
-- changes.
--
-- TODO try to rewrite use Control.Monad.State

eeHitToStream :: Monad m => E.Enumeratee VerboseHit [HitStream z] m a
eeHitToStream = EL.mapAccum go (AliGo "" "" '?') where
  go AliGo{..} vh@VerboseHit{..} = (AliGo vhCM vhScaffold vhStrand,
    [StreamCM vhCM | aliCM /= vhCM] ++
    [StreamScaffold vhScaffold | aliCM /= vhCM || aliScaffold /= vhScaffold] ++
    [StreamStrand vhStrand | aliCM /= vhCM || aliScaffold /= vhScaffold || aliStrand /= vhStrand] ++
    [StreamVerboseHit vh]
    )

-- | Flattens a stream from a list of lists to a single list. After this point,
-- you probably want to insert elements into the stream, then flatten again.

eeFlattenStream :: Monad m => E.Enumeratee [HitStream z] (HitStream z) m a
eeFlattenStream = EL.concatMap id

-- | If the 'HitStream' contains 'StreamInsertion's that are an instance of
-- 'Show', this provides a default method to turn the stream into a bytestring.
--
-- TODO add some newline characters for good measure
--
-- TODO on end-of-stream, we should print out "//"

eeStreamToByteString :: (Monad m, Show z) => StreamToByteString m z
eeStreamToByteString = EL.map f where
  f StreamVerboseHit{..} = showVerboseHit streamVerboseHit `BS.snoc` '\n'
  f StreamCM{..} = showCM streamCM `BS.append` "\n\n"
  f StreamScaffold{..} = showScaffold streamScaffold `BS.append` "\n\n"
  f StreamStrand{..} = showStrand streamStrand `BS.append` "\n\n"
  f StreamInsertion{..} = BS.pack . show $ streamInsertion

eeStreamToByteString' :: (Monad m) => StreamToByteString m ()
eeStreamToByteString' = eeStreamToByteString

type StreamToByteString m z = forall a . E.Enumeratee (HitStream z) BS.ByteString m a
