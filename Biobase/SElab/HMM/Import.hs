
-- | Import HMMER3 HMM models.

module Biobase.SElab.HMM.Import
  ( conduitHMM
  , fromFile
  , parseHMM
  ) where

import           Control.Applicative ( (<|>), pure, (<$>), (<$), (<*>), (*>), (<*) )
import           Control.Lens hiding ((|>))
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
import           Data.Default
import           Data.Sequence ((|>))
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

import           Biobase.Primary
import           Biobase.Types.Accession (Accession(..))
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray as PA hiding (map)

import           Biobase.SElab.Common.Parser
import           Biobase.SElab.HMM.Types



-- | Simple import of HMMs

fromFile :: FilePath -> IO [HMM xfam]
fromFile file = runResourceT $ sourceFile file $= conduitHMM $$ consume

-- |

conduitHMM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m (HMM xfam)
conduitHMM = decodeUtf8 =$= conduitParserEither (parseHMM <?> "HMM parser") =$= awaitForever (either (error . show) (yield . snd))

-- | Parse the header of an HMM, and return the partially filled HMM and
-- a ByteString with the non-parsed remainder.

parseHMMHeader :: AT.Parser (HMM xfam, ByteString)
parseHMMHeader = do
  return undefined

parseHMMBody :: HMM xfam -> AT.Parser (HMM xfam)
parseHMMBody hmm = do
  return undefined

-- |
--
-- NOTE the idea of filling with @999999@ is that if we run the HMM, then any
-- score bugs will yield weird results that show up immediately.

parseHMM :: AT.Parser (HMM xfam)
parseHMM = do
  v <- acceptedVersion
  let hmm' = version .~ v $ def
  ls <- hmmHeader `manyTill` "HMM"
  let hmm = L.foldl' (\a l -> l a) hmm' ls
  eolS
  eolS
  l  <- component0
  ls <- (component (length $ l^._2)) `manyTill` "//"
  AT.try AT.skipSpace
  return
    $ set matchScores      (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._2)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._2:map (view (_2._1)) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ set insertScores     (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._3)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._3:map (view  _3    ) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ set transitionScores (PA.fromAssocs (Z:.0:.Letter 0) (Z:.(PInt $ length ls):.(Letter . subtract 1 . length $ l^._4)) 999999
                                          [((Z:.s:.k),Bitscore v) | (s,vs) <- zip [0..] (l^._4:map (view  _4    ) ls), (k,v) <- zip [Letter 0 ..] vs ])
    $ hmm

acceptedVersion :: AT.Parser (Text,Text)
acceptedVersion = (,) <$> vOk <* AT.skipSpace <*> eolS <?> "accepted Version" where
    vOk = "HMMER3/b" <|> "HMMER3/f"

hmmHeader :: AT.Parser (HMM xfam -> HMM xfam)
hmmHeader = AT.choice
  [ set name                    <$ "NAME"  <*> eolS <?> "name"
  , set accession . Accession   <$ "ACC"   <*> eolS <?> "hmmAccession"
  , set description             <$ "DESC"  <*> eolS <?> "description"
  , set modelLength             <$ "LENG"  <*> eolN <?> "leng"
  , set maxInstanceLen . Just   <$ "MAXL"  <*> eolN <?> "maxl"
  , set alphabet                <$ "ALPH"  <*> eolS <?> "alph"
  , set referenceAnno           <$ "RF"    <*> eolB <?> "rf"
  , set consensusStruc          <$ "CS"    <*> eolB <?> "cs"
  , set consensusRes            <$ "CONS"  <*> eolB <?> "cons"
  , set alignColMap             <$ "MAP"   <*> eolB <?> "map"
  , set modelMask               <$ "MM"    <*> eolB <?> "mm"
  , set date                    <$ "DATE"  <*> eolS <?> "date"
  , set nseq . Just             <$ "NSEQ"  <*> eolN <?> "nseq"
  , set effnseq . Just          <$ "EFFN"  <*> eolD <?> "effn"
  , set chksum  . Just          <$ "CKSUM" <*> eolN <?> "cksum"
  , (\l r -> set gatheringTh   (Just (l,r))) <$ "GA" <*> ssD <*> ssD <* eolS
  , (\l r -> set trustedCutoff (Just (l,r))) <$ "TC" <*> ssD <*> ssD <* eolS
  , (\l r -> set noiseCutoff   (Just (l,r))) <$ "NC" <*> ssD <*> ssD <* eolS
  , (\l r -> set msv     (Just (l,r))) <$ "STATS LOCAL MSV"     <*> ssD <*> ssD <* eolS
  , (\l r -> set viterbi (Just (l,r))) <$ "STATS LOCAL VITERBI" <*> ssD <*> ssD <* eolS
  , (\l r -> set forward (Just (l,r))) <$ "STATS LOCAL FORWARD" <*> ssD <*> ssD <* eolS
  , (\s -> over commandLineLog (|>s))  <$ "COM"                 <*> eolS <?> "com"
  , (\x ->   over unknownLines (|> x)) <$> AT.takeWhile1 (/='\n') <* AT.take 1
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
component k = (,,,) <$> ident <*> (matches <?> "matches") <*> inserts <*> moves <?> "component" where
  ident   = AT.skipSpace *> (error "COMPO parsed in component" <$ "COMPO" <|> AT.decimal) <?> "ident"
  matches = matchHMM <|> matchSubHMM <?> "matchHMM-CM"
  matchHMM    = (,,,,,) <$> count k ssD <*> melMAP <*> pure ' ' <*> melRF <*> melCS <*> pure ' ' <* (AT.endOfLine <?> "eol") <?> "matchHMM"
  matchSubHMM = (,,,,,) <$> count k ssD <*> melMAP <*> melCONS  <*> melRF <*> melCS <*> melStruc <* (AT.endOfLine <?> "eol") <?> "matchSubHMM" -- SubHMM of a CM, not a CM!
  inserts = count k ssD <* AT.endOfLine <?> "inserts"
  moves   = count 7 ssD' <* AT.endOfLine <?> "moves"
  melMAP   = skipHorizSpace *> (AT.decimal <|> (0 <$ "-")) <?> "melMAP"
  melCONS  = skipHorizSpace *> AT.anyChar <?> "melCONS"
  melRF    = skipHorizSpace *> AT.anyChar <?> "melRF"
  melCS    = skipHorizSpace *> AT.anyChar <?> "melCS"
  melStruc = skipHorizSpace *> AT.anyChar <?> "melSTRUC"   -- not defined in the Userguide!
  skipHorizSpace = AT.skipWhile AT.isHorizontalSpace

type Component0 = (Int,  [Double]                    , [Double], [Double])

-- | A Component line. Index (starting with 1, zero is COMPO). Then comes the
-- match line with the scores, a MAP annotation, consensus residue, reference
-- annotation, and consensus structure. HMMer HMMs don't have a consensus
-- structure.

type Component  = (Int, ([Double],Int,Char,Char,Char,Char), [Double], [Double])



-- | Simple test for the HMMer parser.

test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "src/test.hmm" $= conduitHMM $$ consume -- CB.lines $= CL.sequence parseHMM3 $$ consume -- sinkHandle stdout
  print xs

hmmFromFile :: FilePath -> IO [HMM ()]
hmmFromFile fp = runResourceT $ sourceFile fp $= conduitHMM $$ consume

