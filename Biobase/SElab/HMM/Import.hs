
-- | Import HMMER3 HMM models.

module Biobase.SElab.HMM.Import where

import           Control.Applicative ( (<|>), pure, (<$>), (<$), (<*>), (*>), (<*) )
import           Control.Lens hiding ((|>))
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Attoparsec.ByteString (count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.Char (isSpace,isAlpha,isDigit)
import           Data.Char.Util
import           Data.Default
import           Data.Sequence ((|>))
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Data.Vector.Unboxed (fromList)
import           Debug.Trace
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as VU
import           System.FilePath (takeExtension)
import           Control.DeepSeq (($!!))

import           Biobase.Primary
import           Biobase.Types.Accession (Accession(..))
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray as PA hiding (map)

import           Biobase.SElab.Common.Parser
import           Biobase.SElab.HMM.Types




-- | Simple convenience function for parsing HMM's without a lot of
-- fancyness.

hmmFromFile :: FilePath -> IO [HMM ()]
hmmFromFile fp = do
  bs <- BSC.readFile fp
  case AB.parseOnly (many1 parseHMM <* AB.endOfInput) bs of
    Left err -> error err
    Right xs -> return xs

-- |
--
-- NOTE the idea of filling with @999999@ is that if we run the HMM, then any
-- score bugs will yield weird results that show up immediately.

parseHMM :: ABC.Parser (HMM xfam)
parseHMM = do
  pre <- parsePreHMM
  bdy <- parseHMMBody pre
  ABC.endOfLine
  return bdy

-- | Parse the header of an HMM, and return the partially filled HMM and
-- a ByteString with the non-parsed remainder.

parsePreHMM :: ABC.Parser (HMM xfam) -- (HMM xfam, Text)
parsePreHMM = do
  v <- acceptedVersion
  let hmm' = version .~ v $ def
  ls <- hmmHeader `manyTill` "HMM"
  let hmm = L.foldl' (\a l -> l a) hmm' ls
  eolS
  eolS
  -- remainder <- ABC.takeText
  return hmm -- (hmm, remainder)

parseHMMBody :: HMM xfam -> ABC.Parser (HMM xfam)
parseHMMBody hmm = do
  l  <- component0
  ls <- (component (length $ l^._2)) `manyTill` "//"
  ABC.skipSpace
  return
    $!! set matchScores      (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._2)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._2:map (view (_2._1)) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ set insertScores     (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._3)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._3:map (view  _3    ) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ set transitionScores (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._4)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._4:map (view  _4    ) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ hmm

acceptedVersion :: ABC.Parser (Text,Text)
acceptedVersion = (,) <$> (decodeUtf8 <$> vOk) <* ABC.skipSpace <*> eolT <?> "accepted Version" where
    vOk = "HMMER3/b" <|> "HMMER3/f"

hmmHeader :: ABC.Parser (HMM xfam -> HMM xfam)
hmmHeader = ABC.choice
  [ set name                    <$ "NAME"  <*> eolT <?> "name"
  , set accession . Accession   <$ "ACC"   <*> eolT <?> "hmmAccession"
  , set description             <$ "DESC"  <*> eolT <?> "description"
  , set modelLength             <$ "LENG"  <*> eolN <?> "leng"
  , set maxInstanceLen . Just   <$ "MAXL"  <*> eolN <?> "maxl"
  , set alphabet                <$ "ALPH"  <*> eolT <?> "alph"
  , set referenceAnno           <$ "RF"    <*> eolB <?> "rf"
  , set consensusStruc          <$ "CS"    <*> eolB <?> "cs"
  , set consensusRes            <$ "CONS"  <*> eolB <?> "cons"
  , set alignColMap             <$ "MAP"   <*> eolB <?> "map"
  , set modelMask               <$ "MM"    <*> eolB <?> "mm"
  , set date                    <$ "DATE"  <*> eolT <?> "date"
  , set nseq . Just             <$ "NSEQ"  <*> eolN <?> "nseq"
  , set effnseq . Just          <$ "EFFN"  <*> eolD <?> "effn"
  , set chksum  . Just          <$ "CKSUM" <*> eolN <?> "cksum"
  , (\l r -> set gatheringTh   (Just (l,r))) <$ "GA" <*> ssD <*> ssD <* eolS
  , (\l r -> set trustedCutoff (Just (l,r))) <$ "TC" <*> ssD <*> ssD <* eolS
  , (\l r -> set noiseCutoff   (Just (l,r))) <$ "NC" <*> ssD <*> ssD <* eolS
  , (\l r -> set msv     (Just (l,r))) <$ "STATS LOCAL MSV"     <*> ssD <*> ssD <* eolS
  , (\l r -> set viterbi (Just (l,r))) <$ "STATS LOCAL VITERBI" <*> ssD <*> ssD <* eolS
  , (\l r -> set forward (Just (l,r))) <$ "STATS LOCAL FORWARD" <*> ssD <*> ssD <* eolS
  , (\s -> over commandLineLog (|> decodeUtf8 s)) <$ "COM"                 <*> eolS <?> "com"
  , (\x ->   over unknownLines (|> decodeUtf8 x)) <$> ABC.takeWhile1 (/='\n') <* ABC.take 1
  ] <?> "hmmHeader"

-- | TODO

component0 :: ABC.Parser Component0
component0 = (,,,) <$> ident <*> matches <*> inserts <*> moves <?> "COMPO/0" where
  ident   = ABC.skipSpace *> (0 <$ "COMPO") <?> "ident" -- optional
  matches = manyTill ssD  ABC.endOfLine <?> "matches"   -- optional
  inserts = manyTill ssD  ABC.endOfLine <?> "inserts"
  moves   = count 7 ssD' <* ABC.endOfLine <?> "moves"

-- | Parse components. Matches come with annotations. These depend on the specific model.

component :: Int -> ABC.Parser Component
component k = (,,,) <$> ident <*> (matches <?> "matches") <*> inserts <*> moves <?> "component" where
  ident   = ABC.skipSpace *> (error "COMPO parsed in component" <$ "COMPO" <|> ABC.decimal) <?> "ident"
  matches = matchHMM <|> matchSubHMM <?> "matchHMM-CM"
  matchHMM    = (,,,,,) <$> count k ssD <*> melMAP <*> pure ' ' <*> melRF <*> melCS <*> pure ' ' <* (ABC.endOfLine <?> "eol") <?> "matchHMM"
  matchSubHMM = (,,,,,) <$> count k ssD <*> melMAP <*> melCONS  <*> melRF <*> melCS <*> melStruc <* (ABC.endOfLine <?> "eol") <?> "matchSubHMM" -- SubHMM of a CM, not a CM!
  inserts = count k ssD <* ABC.endOfLine <?> "inserts"
  moves   = count 7 ssD' <* ABC.endOfLine <?> "moves"
  melMAP   = skipHorizSpace *> (ABC.decimal <|> (0 <$ "-")) <?> "melMAP"
  melCONS  = skipHorizSpace *> ABC.anyChar <?> "melCONS"
  melRF    = skipHorizSpace *> ABC.anyChar <?> "melRF"
  melCS    = skipHorizSpace *> ABC.anyChar <?> "melCS"
  melStruc = skipHorizSpace *> ABC.anyChar <?> "melSTRUC"   -- not defined in the Userguide!
  skipHorizSpace = ABC.skipWhile (ABC.isHorizontalSpace . c2w8)

type Component0 = (Int,  [Double]                    , [Double], [Double])

-- | A Component line. Index (starting with 1, zero is COMPO). Then comes the
-- match line with the scores, a MAP annotation, consensus residue, reference
-- annotation, and consensus structure. HMMer HMMs don't have a consensus
-- structure.

type Component  = (Int, ([Double],Int,Char,Char,Char,Char), [Double], [Double])



