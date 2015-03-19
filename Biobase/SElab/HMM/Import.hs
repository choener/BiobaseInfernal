{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Import HMMER3 HMM models.

module Biobase.SElab.HMM.Import
  ( parseHMM
  , conduitHMM
  ) where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Resource (runResourceT,MonadThrow)
import           Data.Attoparsec.ByteString (count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.Char (isSpace,isAlpha,isDigit)
import           Data.Conduit.Attoparsec (conduitParserEither)
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit.Text (decodeUtf8)
import           Data.Conduit (yield,awaitForever,(=$=),Conduit,($$),($=))
import           Data.Conduit.Zlib (ungzip)
import           Data.Default.Class
import           Data.Text (Text)
import           Data.Vector.Unboxed (fromList)
import           Debug.Trace
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Attoparsec.Text as AT
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import           System.FilePath (takeExtension)

import           Data.PrimitiveArray as PA hiding (map)

import           Biobase.SElab.Bitscore
import           Biobase.SElab.Common.Parser
import           Biobase.SElab.HMM
import           Biobase.SElab.Types



-- |

conduitHMM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m HMM
conduitHMM = decodeUtf8 =$= conduitParserEither (parseHMM <?> "HMM parser") =$= awaitForever (either (error . show) (yield . snd)) where

-- |
--
-- NOTE the idea of filling with @999999@ is that if we run the HMM, then any
-- score bugs will yield weird results that show up immediately.

parseHMM :: AT.Parser HMM
parseHMM = do
  v <- acceptedVersion
  let hmm' = version .~ v $ def
  -- hmm' <- (\v d -> set version (v,T.init d) def) <$ AT.takeTill (=='[') <*> AT.takeTill (=='|') <*> eolS <?> "hmmVersion"
  ls <- hmmHeader `manyTill` "HMM"
  let hmm = L.foldl' (\a l -> l a) hmm' ls
  eolS
  eolS
  l  <- component0
  ls <- (component (length $ l^._2)) `manyTill` "//"
  AT.skipSpace
  return
    $ set matchScores      (PA.fromAssocs (Z:.0:.0) (Z:.length ls:.(length $ l^._2)-1) 999999 [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._2:map (view (_2._1)) ls), (k,v) <- zip [0..] vs ])
    $ set insertScores     (PA.fromAssocs (Z:.0:.0) (Z:.length ls:.(length $ l^._3)-1) 999999 [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._3:map (view  _3    ) ls), (k,v) <- zip [0..] vs ])
    $ set transitionScores (PA.fromAssocs (Z:.0:.0) (Z:.length ls:.(length $ l^._4)-1) 999999 [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._4:map (view  _4    ) ls), (k,v) <- zip [0..] vs ])
    $ hmm

acceptedVersion :: AT.Parser (Text,Text)
acceptedVersion = (,) <$> vOk <* AT.skipSpace <*> eolS <?> "accepted Version" where
    vOk = "HMMER3/b" <|> "HMMER3/f"

hmmHeader :: AT.Parser (HMM -> HMM)
hmmHeader = AT.choice
  [ set name            <$> "NAME"  ..*> eolS <?> "name"
  , set accession       <$> "ACC"   ..*> eolS <?> "hmmAccession"
  , set description     <$> "DESC"  ..*> eolS <?> "description"
  , id                  <$  "LENG"  ..*> eolS <?> "leng"   -- TODO
  , set alph            <$> "ALPH"  ..*> eolS <?> "alph"
  , set rf              <$> "RF"    ..*> eolB <?> "rf"
  , set cs              <$> "CS"    ..*> eolB <?> "cs"
  , id                  <$  "MAP"   ..*> eolB <?> "map"    -- TODO
  , set date            <$> "DATE"  ..*> eolS <?> "date"
  , set nseq . Just     <$> "NSEQ"  ..*> eolN <?> "nseq"
  , set effnseq . Just  <$> "EFFN"  ..*> eolD <?> "effn"
  , set chksum  . Just  <$> "CKSUM" ..*> eolN <?> "cksum"
  , id                  <$  "GA"    ..*> eolS <?> "ga"      -- TODO
  , id                  <$  "TC"    ..*> eolS <?> "tc"      -- TODO
  , id                  <$  "NC"    ..*> eolS <?> "nc"      -- TODO
  , (\l r -> set msv     (Just (l,r))) <$ "STATS LOCAL MSC"     <*> ssD <*> ssD <* eolS
  , (\l r -> set viterbi (Just (l,r))) <$ "STATS LOCAL VITERBI" <*> ssD <*> ssD <* eolS
  , (\l r -> set forward (Just (l,r))) <$ "STATS LOCAL FORWARD" <*> ssD <*> ssD <* eolS
  , (\x -> trace ("HMM Parser: unknown line:" ++ T.unpack x) id) <$> AT.takeTill (=='\n') <* AT.take 1
  ] <?> "hmmHeader"

-- | TODO

component0 :: AT.Parser Component0
component0 = (,,,) <$> ident <*> matches <*> inserts <*> moves <?> "COMPO/0" where
  ident   = AT.skipSpace *> (0 <$ "COMPO") <?> "ident" -- optional
  matches = manyTill ssD  AT.endOfLine <?> "matches"   -- optional
  inserts = manyTill ssD  AT.endOfLine <?> "inserts"
  moves   = count 7 ssD' <* AT.endOfLine <?> "moves"

-- | Parse components. Matches come with annotations. These depend on the specific model.

component :: Int -> AT.Parser Component
component k = (,,,) <$> ident <*> matches <*> inserts <*> moves <?> "component" where
  ident   = AT.skipSpace *> (error "COMPO parsed in component" <$ "COMPO" <|> AT.decimal) <?> "ident"
  matches = (,,,,) <$> count k ssD <*> (ssN <|> 0 <$ ssS <?> "MAP") <*> (ssC <?> "CONS") <*> (ssC <?> "RF") <*> (ssC <* AT.endOfLine <|> '.' <$ AT.endOfLine) <?> "matches"
  inserts = traceShow k (count k ssD <* AT.endOfLine <?> "inserts")
  moves   = count 7 ssD' <* AT.endOfLine <?> "moves"
  brr x = traceShow x x

type Component0 = (Int,  [Double]                    , [Double], [Double])

-- | A Component line. Index (starting with 1, zero is COMPO). Then comes the
-- match line with the scores, a MAP annotation, consensus residue, reference
-- annotation, and consensus structure. HMMer HMMs don't have a consensus
-- structure.

type Component  = (Int, ([Double],Int,Char,Char,Char), [Double], [Double])



-- | Simple test for the HMMer parser.

test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "devel/test.hmm" $= conduitHMM $$ consume -- CB.lines $= CL.sequence parseHMM3 $$ consume -- sinkHandle stdout
  print xs

