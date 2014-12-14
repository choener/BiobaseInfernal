{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ViewPatterns #-}

-- | Parses text-based covariance-model descriptions. This parser is
-- Utf8-aware.

module Biobase.SElab.CM.Import where

import           Control.Applicative
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Resource (runResourceT,MonadThrow)
import           Data.Array.Repa.Index
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit.Attoparsec (conduitParserEither)
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit.Text (decodeUtf8)
import           Data.Conduit (yield,awaitForever,(=$=),Conduit,($$),($=))
import           Data.Default.Class
import           Data.Function (on)
import           Data.Ord (comparing)
import           Debug.Trace
import qualified Data.Attoparsec.Text as AT
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import           Biobase.Primary.Nuc.RNA
import qualified Data.PrimitiveArray as PA
import qualified Data.PrimitiveArray.Zero as PA

import           Biobase.SElab.Bitscore
import           Biobase.SElab.CM
import           Biobase.SElab.Common.Parser
import           Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.HMM.Import as HMM



-- | Stream a 'ByteString' into 'CM's.

conduitCM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m CM
conduitCM = decodeUtf8 =$= conduitParserEither (parseCM <?> "CM parser") =$= awaitForever (either (error . show) (yield . snd))

-- | Parser for covariance models (CMs).

parseCM :: AT.Parser CM
parseCM = do
  cm' <- (\v d -> set version (v,T.init d) def) <$ "INFERNAL1/a [" <*> AT.takeTill (=='|') <*> eolS <?> "cmVersion"
  ls <- manyTill cmHeader "CM"
  let cm = L.foldl' (\a l -> l a) cm' ls
  eolS
  ns' <- manyTill node "//"
  let maxState = maximum $ ns' ^.. folded . _2 . folded . sid
  let ns = V.fromList [ n & nstates .~ (VU.fromList $ map (view sid) ss) | (n,ss) <- ns' ]
  eolS
  cmhmm <- HMM.parseHMM <|> pure def  -- if there is no HMM, then return an empty one
  return
    $ set states States
        { _sTransitions     = PA.fromAssocs (Z:.0:.0) (Z:.0:.5) (-1,def)
                            . concatMap (\s -> [((Z:.s^.sid:.k),(i,e)) | k <- [0..5]
                                                                       | (i,e) <- if s^.sType == sB then map (,0) $ s^..sChildren.both
                                                                                                    else zip (uncurry enumFromTo $ s^.sChildren) (VU.toList $ s^.transitions)
                                                ])
                            $ ns'^..folded._2.folded
        , _sPairEmissions   = PA.fromAssocs (Z:.0:.A:.A) (Z:.maxState:.U:.U) def
        -- TODO the zip3 seems to be a bug?
                            . concatMap (\s -> [((Z:.s^.sid:.n1:.n2),e) | emitsPair (s^.sType), (n1,n2,e) <- zip3 acgu acgu (VU.toList $ s^.emissions)]) $ ns'^..folded._2.folded
        , _sSingleEmissions = PA.fromAssocs (Z:.0:.A) (Z:.maxState:.U) def
                            . concatMap (\s -> [((Z:.s^.sid:.nt),e) | emitsSingle (s^.sType), (nt,e) <- zip acgu (VU.toList $ s^.emissions)] ) $ ns' ^.. folded . _2 . folded
        , _sStateType       = PA.fromAssocs (Z:.0) (Z:.maxState) sIllegal . map ((,) <$> ((Z:.) <$> view sid) <*> view sType) $ ns' ^.. folded . _2 . folded
        }
    $ set hmm cmhmm
    $ set nodes ns
    $ cm

-- | Parse CM header information.
--
-- TODO not all header information is stored in the structure yet.

cmHeader :: AT.Parser (CM -> CM)
cmHeader = AT.choice
  [ set name            <$> "NAME"      ..*> eolS <?> "name"
  , set accession       <$> "ACC"       ..*> eolS <?> "hmmAccession"
  , set description     <$> "DESC"      ..*> eolS <?> "description"
  , id                  <$  "STATES"    ..*> eolN <?> "states"
  , id                  <$  "NODES"     ..*> eolN <?> "nodes"
  , set clen            <$> "CLEN"      ..*> eolN <?> "clen"
  , set w               <$> "W"         ..*> eolN <?> "w"
  , set alph            <$> "ALPH"      ..*> eolS <?> "alph"
  , id                  <$  "RF"        ..*> eolB <?> "rf"
  , id                  <$  "CONS"      ..*> eolB <?> "cons"
  , id                  <$  "MAP"       ..*> eolB <?> "map"
  , set date            <$> "DATE"      ..*> eolS <?> "date"
  , set pbegin          <$> "PBEGIN"    ..*> eolD <?> "pbegin"
  , set pend            <$> "PEND"      ..*> eolD <?> "pend"
  , set wbeta           <$> "WBETA"     ..*> eolD <?> "wbeta"
  , set qdbBeta1        <$> "QDBBETA1"  ..*> eolD <?> "qdbbeta1"
  , set qdbBeta2        <$> "QDBBETA2"  ..*> eolD <?> "qdbbeta2"
  , set n2Omega         <$> "N2OMEGA"   ..*> eolD <?> "n2omega"
  , set n3Omega         <$> "N3OMEGA"   ..*> eolD <?> "n3omega"
  , set elseLF          <$> "ELSELF"    ..*> eolD <?> "elself"
  , set nseq            <$> "NSEQ"      ..*> eolN <?> "nseq"
  , set effn            <$> "EFFN"      ..*> eolD <?> "effn"
  , set cksum           <$> "CKSUM"     ..*> eolN <?> "cksum"
  , set ga              <$> "GA"        ..*> eolD <?> "ga"
  , set tc              <$> "TC"        ..*> eolD <?> "tc"
  , ecm "ECMLC"
  , ecm "ECMGC"
  , ecm "ECMLI"
  , ecm "ECMGI"
  , (\[a,b] -> set efp7gf (a,b)) <$> "EFP7GF" ..*> AT.count 2 ssD <* eolS <?> "efp7gf"
  , set nullModel . VU.fromList . map Bitscore <$> "NULL"   ..*> AT.count 4 ssD <* eolS <?> "null"
  , (\s -> over commandLog (++[s])) <$> "COM"  ..*> eolS <?> "com"
  , (\x -> trace ("HMM Parser: unknown line:" ++ T.unpack x) id) <$> AT.takeTill (=='\n') <* AT.take 1
  ] <?> "cmHeader"
  where
    ecm s = (\a b c d e f -> set ecmlc (EValueParams a b c d e f)) <$> s ..*> ssD <*> ssD <*> ssD <*> ssN <*> ssN <*> ssD <* eolS <?> "ecm parser"

-- | Parse a node together with the attached states.

node :: AT.Parser (Node, [State])
node = (,) <$> aNode <*> AT.many1 aState where
  aNode  = Node VU.empty <$ AT.skipSpace <* "[ " <*> anType <*> ssN <* AT.skipSpace <* "]" <*> ssN_ <*> ssN_ <*> ssC <*> ssC <*> ssC <*> ssC
  anType :: AT.Parser NodeType
  anType = AT.choice [ nBif  <$ "BIF" , nMatP <$ "MATP", nMatL <$ "MATL", nMatR <$ "MATR"
                     , nBegL <$ "BEGL", nBegR <$ "BEGR", nRoot <$ "ROOT", nEnd  <$ "END" ]
  aState = do AT.skipSpace
              _sType     <- asType
              _sid       <- ssN
              _sParents  <- (,) <$> ssZ <*> ssN
              _sChildren <- (,) <$> ssZ <*> ssN
              _sqdb      <- (,,,) <$> ssN <*> ssN <*> ssN <*> ssN
              _transitions <- if | _sType==sB -> pure VU.empty
                                 | otherwise  -> VU.fromList <$> AT.count (_sChildren^._2) (Bitscore <$> ssD')
              _emissions   <- if | _sType==sMP -> VU.fromList <$> AT.count 16 (Bitscore <$> ssD)
                                 | _sType `elem` [sML,sMR,sIL,sIR] -> VU.fromList <$> AT.count 4 (Bitscore <$> ssD)
                                 | otherwise -> pure VU.empty
              eolS
              return State{..}
  asType :: AT.Parser StateType
  asType = AT.choice [ sD <$ "D", sMP <$ "MP", sML <$ "ML", sMR <$ "MR"
                     , sIL <$ "IL", sIR <$ "IR", sS <$ "S", sE <$ "E", sB <$ "B" ]

-- | Read a list of CMs from a given filename.

cmFromFile :: FilePath -> IO [CM]
cmFromFile fp = runResourceT $ sourceFile fp $= conduitCM $$ consume

