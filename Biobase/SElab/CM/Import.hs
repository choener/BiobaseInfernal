{- LANGUAGE BangPatterns #-}
{- LANGUAGE DoAndIfThenElse #-}
{- LANGUAGE LambdaCase #-}
{- LANGUAGE MultiWayIf #-}
{- LANGUAGE NoMonomorphismRestriction #-}
{- LANGUAGE OverloadedStrings #-}
{- LANGUAGE OverloadedStrings #-}
{- LANGUAGE ParallelListComp #-}
{- LANGUAGE PatternGuards #-}
{- LANGUAGE RecordWildCards #-}
{- LANGUAGE ScopedTypeVariables #-}
{- LANGUAGE TemplateHaskell #-}
{- LANGUAGE TupleSections #-}
{- LANGUAGE ViewPatterns #-}
{- LANGUAGE ViewPatterns #-}

-- | Parses text-based covariance-model descriptions. This parser is
-- Utf8-aware.

module Biobase.SElab.CM.Import where

{-

import           Control.Monad
import           Data.Conduit (yield,awaitForever,(=$=),Conduit,($$),($=))
import           Data.Default.Class
import           Data.Function (on)
import           Data.Ord (comparing)
import           Debug.Trace
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.Nuc.RNA
import           Data.PrimitiveArray as PA hiding (map)

import           Biobase.SElab.Bitscore
import           Biobase.SElab.CM
import           Biobase.SElab.Common.Parser
import           Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.HMM.Import as HMM

-}

import           Control.Monad (forM_)
import           Control.Applicative ( (<|>), pure, (<$>), (<$), (<*>), (<*) )
import           Control.Lens
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Resource (runResourceT,MonadThrow)
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit
import           Data.Conduit.Attoparsec (conduitParserEither)
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit.Text (decodeUtf8)
import           Data.Default
import           Data.Text (unpack)
import           Data.Vector.Generic (fromList,empty,toList)
import           Debug.Trace
import qualified Data.Attoparsec.Text as AT
import qualified Data.List as L
import qualified Data.Text as T

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Accession (Accession(Accession))
import           Data.PrimitiveArray hiding (fromList,map,toList)

import           Biobase.SElab.Bitscore
import           Biobase.SElab.CM.Types
import           Biobase.SElab.Common.Parser
import           Biobase.SElab.HMM.Import (parseHMM)
import           Biobase.SElab.HMM.Types (HMM)
import           Biobase.SElab.Types
import qualified Biobase.SElab.CM.Types as CM
import qualified Biobase.SElab.HMM.Types as HMM



-- | Stream a 'ByteString' into 'CM's.
--
-- NOTE Each CM is /always/ followed by the corresponding filter HMM.
-- (Infernal 1.1.1. at least)
--
-- TODO this should yield @Either CM HMM@. Internally we check if part of
-- the stream @[... , CM, HMM, ...]@. We might want to provide a function
-- @mergeCmHmm@ that merges consecutive @CMs@ and @HMMs@ into the @CM@ but
-- still leaves the unmerged ones separately. Maybe we then want the
-- @these@ package, which has @a , b, (a,b)@ style data types.

conduitCM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m CM
conduitCM = decodeUtf8 =$= conduitParserEither (parseCM <?> "CM parser") =$= awaitForever (either (error . show) (yield . snd))

-- | Parser for covariance models (CMs).

parseCM :: AT.Parser CM
parseCM = do
  v <- acceptedVersion
  let cm' = version .~ v $ def
  ls <- manyTill cmHeader "CM"
  let cm = L.foldl' (\a l -> l a) cm' ls
  ns' <- manyTill node "//"
  let maxState = maximum $ ns' ^.. folded . _2 . folded . sid
  let ns = fromList [ n & nstates .~ (fromList $ map (view sid) ss) | (n,ss) <- ns' ]
  AT.try AT.endOfLine
  cmhmm <- parseHMM <|> pure def  -- if there is no HMM, then return an empty one
  return
    $ set states States
        { _sTransitions     = fromAssocs (Z:.0:.0) (Z:.maxState:.5) (-1,def)
                            . concatMap (\s -> [((Z:.s^.sid:.k),(i,e)) | k <- [0..5]
                                                                       | (i,e) <- if s^.sType == B then map (,0) $ s^..sChildren.both
                                                                                                   else zip (uncurry enumFromTo $ s^.sChildren) (toList $ s^.transitions)
                                                ])
                            $ ns'^..folded._2.folded
        , _sPairEmissions   = fromAssocs (Z:.0:.A:.A) (Z:.maxState:.U:.U) def
        -- TODO the zip3 seems to be a bug?
                            . concatMap (\s -> [((Z:.s^.sid:.n1:.n2),e) | emitsPair (s^.sType), (n1,n2,e) <- zip3 acgu acgu (toList $ s^.emissions)]) $ ns'^..folded._2.folded
        , _sSingleEmissions = fromAssocs (Z:.0:.A) (Z:.maxState:.U) def
                            . concatMap (\s -> [((Z:.s^.sid:.nt),e) | emitsSingle (s^.sType), (nt,e) <- zip acgu (toList $ s^.emissions)] ) $ ns' ^.. folded . _2 . folded
        , _sStateType       = fromAssocs (Z:.0) (Z:.maxState) (StateType $ -1) . map ((,) <$> ((Z:.) <$> view sid) <*> view sType) $ ns' ^.. folded . _2 . folded
        }
    $ set hmm cmhmm
    $ set nodes ns
    $ cm

acceptedVersion :: AT.Parser (T.Text,T.Text)
acceptedVersion = (,) <$ "INFERNAL1/a [" <*> AT.takeTill (=='|') <*> eolS <?> "cmVersion"

-- | Parse CM header information.
--
-- TODO not all header information is stored in the structure yet.

cmHeader :: AT.Parser (CM -> CM)
cmHeader = AT.choice
  [ set name          <$ "NAME"     <*> eolS <?> "name"
  , set description   <$ "DESC"     <*> eolS <?> "description"
  , set statesInModel <$  "STATES"  <*> eolN <?> "states"
  , set nodesInModel  <$  "NODES"   <*> eolN <?> "nodes"
  , set clen          <$ "CLEN"     <*> eolN <?> "clen"
  , set w             <$ "W"        <*> eolN <?> "w"
  , set alph          <$ "ALPH"     <*> eolS <?> "alph"
  , set referenceAnno <$  "RF"      <*> eolB <?> "rf"
  , set consensusRes  <$  "CONS"    <*> eolB <?> "cons"
  , set alignColMap   <$  "MAP"     <*> eolB <?> "map"
  , set date          <$ "DATE"     <*> eolS <?> "date"
  , set pbegin        <$ "PBEGIN"   <*> eolD <?> "pbegin"
  , set pend          <$ "PEND"     <*> eolD <?> "pend"
  , set wbeta         <$ "WBETA"    <*> eolD <?> "wbeta"
  , set qdbBeta1      <$ "QDBBETA1" <*> eolD <?> "qdbbeta1"
  , set qdbBeta2      <$ "QDBBETA2" <*> eolD <?> "qdbbeta2"
  , set n2Omega       <$ "N2OMEGA"  <*> eolD <?> "n2omega"
  , set n3Omega       <$ "N3OMEGA"  <*> eolD <?> "n3omega"
  , set elseLF        <$ "ELSELF"   <*> eolD <?> "elself"
  , set nseq          <$ "NSEQ"     <*> eolN <?> "nseq"
  , set effn          <$ "EFFN"     <*> eolD <?> "effn"
  , set cksum         <$ "CKSUM"    <*> eolN <?> "cksum"
  , set ga            <$ "GA"       <*> eolD <?> "ga"
  , set tc            <$ "TC"       <*> eolD <?> "tc"
  , set accession . Accession <$ "ACC" <*> eolS <?> "hmmAccession"
  , ecm "ECMLC"
  , ecm "ECMGC"
  , ecm "ECMLI"
  , ecm "ECMGI"
  , set efp7gf <$> ((,) <$ "EFP7GF" <*> ssD <*> eolD <?> "efp7gf")
  , set nullModel . fromList . map Bitscore <$ "NULL"   <*> AT.count 4 ssD <* eolS <?> "null"
  , (\s -> over commandLineLog (|>s)) <$ "COM"  <*> eolS <?> "com"
  , (\x ->   over unknownLines (|> x)) <$> AT.takeWhile1 (/='\n') <* AT.take 1
  ] <?> "cmHeader"
  where
    ecm s = (\a b c d e f -> set ecmlc (EValueParams a b c d e f)) <$ s <*> ssD <*> ssD <*> ssD <*> ssN <*> ssN <*> ssD <* eolS <?> "ecm parser"

-- | Parse a node together with the attached states.

node :: AT.Parser (Node, [State])
node = (,) <$> aNode <*> AT.many1 aState <?> "node" where
  aNode  =   Node empty <$ AT.skipSpace <* ("[ " <?> "[ ") <*> anType <*> (ssN <?> "node ID") <* AT.skipSpace <* "]"
         <*> (ssN_ <?> "mapL") <*> (ssN_ <?> "mapR") <*> (ssC <?> "consL") <*> (ssC <?> "consR") <*> (ssC <?> "rfL") <*> (eolC <?> "rfR") <?> "aNode"
  anType :: AT.Parser NodeType
  anType = AT.choice [ Bif  <$ "BIF" , MatP <$ "MATP", MatL <$ "MATL", MatR <$ "MATR"
                     , BegL <$ "BEGL", BegR <$ "BEGR", Root <$ "ROOT", End  <$ "END" ] <?> "anType"
  aState = do AT.skipSpace
              _sType     <- asType <?> "asType"
              _sid       <- ssN
              _sParents  <- (,) <$> ssZ <*> ssN
              _sChildren <- (,) <$> ssZ <*> ssN
              _sqdb      <- (,,,) <$> ssN <*> ssN <*> ssN <*> ssN
              _transitions <- if | _sType==B  -> pure empty
                                 | otherwise  -> fromList <$> AT.count (_sChildren^._2) (Bitscore <$> ssD')
              _emissions   <- if | _sType==MP -> fromList <$> AT.count 16 (Bitscore <$> ssD)
                                 | _sType `elem` [ML,MR,IL,IR] -> fromList <$> AT.count 4 (Bitscore <$> ssD)
                                 | otherwise -> pure empty
              eolS
              return State{..}
  asType :: AT.Parser StateType
  asType = AT.choice [ D  <$ "D" , MP <$ "MP", ML <$ "ML", MR <$ "MR"
                     , IL <$ "IL", IR <$ "IR", S  <$ "S" , E  <$ "E" , B <$ "B" ]

-- | Read a list of CMs from a given filename.

fromFile :: FilePath -> IO [CM]
fromFile fp = runResourceT $ sourceFile fp $= conduitCM $$ consume

test = do
  cms <- fromFile "tests/test11.cm"
  forM_ cms $ \cm -> do
    print $ cm ^. unknownLines
