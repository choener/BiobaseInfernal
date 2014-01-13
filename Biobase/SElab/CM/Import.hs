{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses text-based covariance-model descriptions.

module Biobase.SElab.CM.Import where

import           Control.Applicative
import           Control.Lens (view,(^.),(^..),folded)
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Data.Attoparsec.ByteString.Char8 (endOfLine,skipSpace,decimal,double,rational,isEndOfLine,(.*>),signed)
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString,unpack)
import           Data.Char (isSpace,isAlpha,isDigit)
import           Data.Conduit.Attoparsec (conduitParserEither)
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit (yield,awaitForever,(=$=),Conduit,MonadThrow,($$),($=),runResourceT)
import           Data.Conduit.Zlib (ungzip)
import           Data.Vector.Unboxed (fromList)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.Map as M
import qualified Data.Vector.Unboxed as VU (fromList)
import           System.FilePath (takeExtension)

import           Biobase.SElab.CM
import           Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.HMM.Import as HMM

import Debug.Trace



test :: IO ()
test = do
  --xs11 <- runResourceT $ sourceFile "trna.cm" $= parseCM $$ consume
  --xs11 <- fromFile "rfam-all.cm"
  --xs11 <- fromFile "trna.cm"
  xs11 <- fromFile "two.cm"
  print xs11

-- | Helper function to simplify parsing CMs from a (possibly gzipped) file.

fromFile f
  | ext == ".cm.gz" = runResourceT $ sourceFile f $= ungzip =$= parseCM $$ consume
  | ext == ".cm"    = runResourceT $ sourceFile f $=            parseCM $$ consume
  | otherwise       = error $ "can't read from: " ++ f
  where ext = takeExtension f

-- | TODO this parser currently parses only 1.1 models

parseCM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m CM
parseCM = conduitParserEither (go <?> "CM parser") =$= awaitForever (either (error . show) (yield . snd)) where -- CL.sequence (sinkParser go) where
  go = do
    _version     <-  (1,1) <$ "INFERNAL1/a" <* eolS
                 <?> "CM: _version"
    _name        <-  IDD <$> "NAME" ..*> eolS
    _accession   <-  optional $ ACC <$ "ACC" ..*> "RF" <*> decimal <* endOfLine
    _description <-  option "" $  "DESC" ..*> eolS
    states    <- "STATES"   ..*> eolN  -- number of states
    nodes     <- "NODES"    ..*> eolN  -- number of nodes
    clen      <- "CLEN"     ..*> eolN  -- consensus model length (matl + matr + 2*matp)
    _w        <- "W"        ..*> eolN  -- maximum expected size of hit
    _alph     <- "ALPH"     ..*> eolS  -- currently only "RNA"
    _rf       <- option False $ "RF" ..*> eolB
    _cons     <- "CONS"     ..*> eolB
    _mapAnno  <- option False $ "MAP"  ..*> eolB
    _date     <- option ""    $ "DATE" ..*> eolS
    _commandLineLog <- many $ "COM" ..*> eolS
    _pBegin   <- option 0.05 $ "PBEGIN" ..*> eolR
    _pEnd     <- option 0.05 $ "PEND"   ..*> eolR
    _wBeta    <- "WBETA"    ..*> eolR
    _qdbBeta1 <- "QDBBETA1" ..*> eolR
    _qdbBeta2 <- "QDBBETA2" ..*> eolR
    _n2Omega  <- "N2OMEGA"  ..*> eolR
    _n3Omega  <- "N3OMEGA"  ..*> eolR
    _elSelf   <- "ELSELF"   ..*> eolR
    _nseq     <- optional $ "NSEQ"  ..*> eolN
    _effnseq  <- optional $ "EFFN"  ..*> eolR
    _chksum   <- optional $ "CKSUM" ..*> eolN
    _nullModel     <- "NULL" ..*> (VU.fromList <$> count 4 (BitScore <$> ssQ) <* eolS)
    _gathering     <- optional $ "GA" ..*> (BitScore <$> eolR)
    _trustedCutoff <- optional $ "TC" ..*> (BitScore <$> eolR)
    _noiseCutoff   <- optional $ "NC" ..*> (BitScore <$> eolR)
    _efp7gf   <- "EFP7GF"   ..*> ((,) <$> ssD <*> ssD <* eolS) -- count 2 (ssD) <* eolS
    _ecmLC    <- optional $ "ECMLC" ..*> statParam
    _ecmGC    <- optional $ "ECMGC" ..*> statParam
    _ecmLI    <- optional $ "ECMLI" ..*> statParam
    _ecmGI    <- optional $ "ECMGI" ..*> statParam
    "CM" *> endOfLine
    ns <- manyTill parseNode "//" <* endOfLine <?> "nodes"
    let _nodes      = M.fromList $ map (\(n,_) -> (n^.nID,n)) ns
    let _states     = M.fromList $ concatMap (map (\s -> (s^.sID,s)) . snd) ns
    unless (M.size _nodes  == nodes ) $ error "Number of nodes does not match header information"
    unless (M.size _states == states) $ error "Number of states does not match header information"
    let l = 2*(length $ _states^..folded._MP) + (length $ _states^..folded._ML) + (length $ _states^..folded._MR)
    unless (l==clen) (error $ "consensus length does not match header information" ++ show (l,clen))
    let l = 2*(length $ _nodes^..folded._MatP) + (length $ _nodes^..folded._MatL) + (length $ _nodes^..folded._MatR)
    unless (l==clen) (error $ "consensus length does not match header information" ++ show (l,clen))
    let _localBegin = M.empty
    let _localEnd   = M.empty
    hmm <- manyTill infoLine "//" <* endOfLine <?> "hmm"
    let _hmm = Nothing
--    rest <- takeByteString
--    error $ ("\n"++) $ L.take 100 $ BS.unpack rest
    return CM{..}

-- | Parses nodes, including the states belonging to each node.

parseNode = do
  skipSpace
  "[ "
  nctor <-  Root <$ "ROOT" <|> Bif  <$ "BIF"  <|> End  <$ "END"
        <|> BegL <$ "BEGL" <|> BegR <$ "BEGR" <|> MatL <$ "MATL" <|> MatR <$ "MATR" <|> MatP <$ "MATP"
  nid <- NodeID <$> ssN
  skipSpace
  "]"
  colL <- xDecimal  -- left consensus column from alignment ('-' or column number) ("MAP")
  colR <- xDecimal  -- right consensus column from alignment ('-' or column number)
  resL <- xString   -- left consensus residue ("CONS")
  resR <- xString   -- right consensus residue
  rfL  <- xString   -- left reference
  rfR  <- xString   -- right reference
  eolS
  ss <- many1 $ parseState nid
  return $ (nctor nid (map (view sID) ss), ss)

-- | Parses individual states. Parsers are grouped according to their
-- ``family''.

parseState nid = skipSpace *> (sde <|> mi <|> mp <|> b)
  where
  sde = do
    sctor <- S <$ "S" <|> D <$ "D" <|> E <$ "E"
    (s,p,tids) <- upDown
    ts <- trans (length tids)
    eolS
    return $ sctor s nid (zip tids ts)
  mi = do
    sctor <- IL <$ "IL" <|> IR <$ "IR" <|> ML <$ "ML" <|> MR <$ "MR"
    (s,p,tids) <- upDown
    ts <- trans (length tids)
    es <- emit 4
    return $ sctor s nid (zip tids ts) (rnaEs es)
  mp = do
    sctor <- MP <$ "MP"
    (s,p,tids) <- upDown
    ts <- trans (length tids)
    es <- emit 16
    return $ sctor s nid (zip tids ts) (rnaPs es)
  b = do
    sctor <- B <$ "B"
    (s,p,l,r) <- (,,,) <$> sid <*> parent <* ssN
                       <*> (StateID <$> ssN) <*> (StateID <$> ssN)
                       <*  ssN <* ssN <* ssN <* ssN
    return $ sctor s nid (l,r)
                -- state id, highest numbered parent, number of parents,
  upDown = (,,) <$> sid <*> parent <* ssN
                -- lowest numbered child, number of children (or right begin for @b@)
                <*> ((\f n -> map StateID . take n $ [f, f+1 ..]) <$> ssZ <*> ssN)
                -- QDB values:
                <*  ssN <* ssN <* ssN <* ssN
  trans c = count c $ skipSpace *> (BitScore <$> double <|> BitScore (-999999) <$ "*")
  emit  c = count c (BitScore <$ skipSpace <*> double)
  sid = StateID <$> ssN
  numTs = ssN
  parent = skipSpace *> (Nothing <$ "-1" <|> Just <$> decimal)
  firstChild = skipSpace *> (Nothing <$ "-1" <|> Just <$> decimal)
  rnaEs = M.fromList . zip "ACGU"
  rnaPs = M.fromList . zip [ (l,r) | l<-"ACGU",r<-"ACGU" ]



-- * Helper functions

ssN = skipSpace *> decimal
ssZ = skipSpace *> signed decimal
ssQ = skipSpace *> rational
ssD = skipSpace *> double
ssString = skipSpace *> ABC.takeTill isSpace

xDecimal = skipSpace *> (Nothing <$ "-" <|> Just <$> decimal)
xString  = skipSpace *> (Nothing <$ "-" <|> Just <$> ABC.takeTill isSpace)

infoLine = (,) <$> ABC.takeWhile isAlpha <* skipSpace <*> takeTill isEndOfLine <* endOfLine

(..*>) s t = s .*> skipSpace *> t

statParam = EValueParams <$> ssD <*> ssD <*> ssD <*> ssN <*> ssN <*> eolD
eolS = takeTill isEndOfLine <* endOfLine
eolR = skipSpace *> rational <* endOfLine
eolD = skipSpace *> double <* endOfLine
eolN = skipSpace *> decimal <* endOfLine
eolZ = skipSpace *> signed decimal <* endOfLine
eolB = skipSpace *> (True <$ "yes" <|> False <$ "no") <* endOfLine

