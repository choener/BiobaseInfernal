
-- | Parses text-based covariance-model descriptions. This parser is
-- Utf8-aware.

module Biobase.SElab.CM.Import where

import           Control.Applicative ( (<|>), pure, (<$>), (<$), (<*>), (<*) )
import           Control.DeepSeq (($!!))
import           Control.Lens
import           Control.Monad (forM_)
import           Control.Monad.IO.Class (MonadIO)
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString)
import           Data.Default
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text (Text)
import           Data.Text (unpack)
import           Data.Vector.Generic (fromList,empty,toList)
import           Data.Vector.Generic.Lens
import           Data.Vector (Vector)
import           Debug.Trace
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import qualified Data.ByteString.Char8 as BSC
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Data.Vector.Generic as VG
import           System.FilePath (takeExtension)
import           System.IO (stdin)

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Accession (Accession(Accession),Rfam,retagAccession)
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray hiding (fromList,map,toList)

import           Biobase.SElab.CM.ModelStructure
import           Biobase.SElab.CM.Types
import           Biobase.SElab.Common.Parser
import           Biobase.SElab.HMM.Import (parseHMM)
import           Biobase.SElab.HMM.Types (HMM)
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

--conduitCM :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m CM
--conduitCM = decodeUtf8 =$= conduitParserEither (parseCM <?> "CM parser") =$= awaitForever (either (error . show) (yield . snd))

-- | Simple convenience function for parsing HMM's without a lot of
-- fancyness.

cmFromFile :: FilePath -> IO [CM]
cmFromFile fp = do
  bs <- BSC.readFile fp
  case ABC.parseOnly (many1 parseCM <* ABC.endOfInput) bs of
    Left err -> error err
    Right xs -> return xs

-- | Parser for covariance models (CMs). Will switch to specialized parsing
-- depending on the model version.

parseCM :: ABC.Parser CM
parseCM = do
  pre <- parsePreCM
  bdy <- parseCMBody pre
  hm  <- parseHMM
  return $ set hmm (over HMM.accession retagAccession hm) bdy

-- |

parsePreCM :: ABC.Parser CM -- (CM, Text)
parsePreCM = do
  v <- acceptedVersion
  let cm' = version .~ v $ def
  ls <- manyTill cmHeader ("CM" <|> "MODEL:")
  let cm = L.foldl' (\a l -> l a) cm' ls
  -- remainder <- ABC.takeText
  return cm -- (cm, remainder)

parseCMBody :: CM -> ABC.Parser CM
parseCMBody cm = do
  let v = cm^.version
  nss <- case v of
          -- parsing of 1.x versions
          (vv,_) | "1."  `T.isPrefixOf` vv -> manyTill node1x "//"
          (vv,_) | "0.7" `T.isPrefixOf` vv -> manyTill node07 "//"
          err -> error $ show err
  ABC.endOfLine  <|> pure ()
  buildCM nss cm def

-- | We have all the parts, just need to fill up the optimized 'States'
-- data structure.
--
-- TODO make sure that @ss@ is ordered by @sid@ and that there are no
-- missing states!

buildCM :: [((PInt () NodeIndex, Node),[(PInt () StateIndex, State)])] -> CM -> HMM Rfam -> ABC.Parser CM
buildCM nss cm cmhmm = do
  let ns = M.fromList $ map fst nss
  let ss = M.fromList $ concatMap snd nss
  let cm' = set hmm cmhmm
          $ set CM.cm (Left $ FlexibleModel { _fmStates = ss, _fmNodes = ns })
          $ cm
  return $!! cm'

acceptedVersion :: ABC.Parser (T.Text,T.Text)
acceptedVersion = (new <?> "new") <|> (old <?> "old") <?> "acceptedVersion"
  where new = (,) <$ "INFERNAL1/a [" <*> (decodeUtf8 <$> ABC.takeTill (=='|') <?> "x-|") <*> (eolT <?> "|->")
        old = (,"") <$ "INFERNAL-1 [" <*> (decodeUtf8 <$> ABC.takeTill (==']') <?> "decode") <* (eolT <?> "endOfLine")

-- | Parse CM header information.
--
-- TODO not all header information is stored in the structure yet.

cmHeader :: ABC.Parser (CM -> CM)
cmHeader = ABC.choice
  [ set name          <$ "NAME"     <*> eolT <?> "name"
  , set description   <$ "DESC"     <*> eolT <?> "description"
  , set statesInModel <$ "STATES"   <*> eolN <?> "states"
  , set nodesInModel  <$ "NODES"    <*> eolN <?> "nodes"
  , set clen          <$ "CLEN"     <*> eolN <?> "clen"
  , set w             <$ "W"        <*> eolN <?> "w"
  , set alph          <$ "ALPH"     <*> eolT <?> "alph"
  , set referenceAnno <$ "RF"       <*> eolB <?> "rf"
  , set consensusRes  <$ "CONS"     <*> eolB <?> "cons"
  , set alignColMap   <$ "MAP"      <*> eolB <?> "map"
  , set date          <$ "DATE"     <*> eolT <?> "date"
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
  , set accession . Accession <$ "ACC" <*> eolT <?> "hmmAccession"
  , ecm "ECMLC"
  , ecm "ECMGC"
  , ecm "ECMLI"
  , ecm "ECMGI"
  , set efp7gf <$> ((,) <$ "EFP7GF" <*> ssD <*> eolD <?> "efp7gf")
  , set nullModel . fromList . map Bitscore <$ "NULL"   <*> ABC.count 4 ssD <* eolS <?> "null"
  , (\s -> over commandLineLog (|> decodeUtf8 s)) <$ "COM"  <*> eolS <?> "com"
  , (\x ->   over unknownLines (|> decodeUtf8 x)) <$> ABC.takeWhile1 (/='\n') <* ABC.take 1
  ] <?> "cmHeader"
  where
    ecm s = (\a b c d e f -> set ecmlc (EValueParams a b c d e f)) <$ s <*> ssD <*> ssD <*> ssD <*> ssN <*> ssN <*> ssD <* eolS <?> "ecm parser"

-- | Parse a node together with the attached states.

node1x :: ABC.Parser ((PInt () NodeIndex, Node), [(PInt () StateIndex, State)])
node1x =
  (\n ss -> (n & _2 . nodeStates .~ (VG.fromList $ map fst ss), ss)) <$> aNode <*> ABC.many1 (aState True) <?> "node1x"
  where
  aNode = (\nty nid mapl mapr conl conr refl refr -> (nid, Node nty empty mapl mapr conl conr refl refr))
          <$ ABC.skipSpace <* ("[ " <?> "[ ")
          <*> anType <*> (ssN <?> "node ID")
          <* ABC.skipSpace <* "]"
          <*> (ssN_ <?> "mapL") <*> (ssN_ <?> "mapR")
          <*> (ssC <?> "consL") <*> (ssC <?> "consR")
          <*> (ssC <?> "rfL") <*> (eolC <?> "rfR") <?> "aNode"

node07 :: ABC.Parser ((PInt () NodeIndex, Node), [(PInt () StateIndex, State)])
node07 =
  -- Update the node with the indices from all the states belonging with
  -- this node.
  (\n ss -> (n & _2 . nodeStates .~ (VG.fromList $ map fst ss), ss)) <$> aNode <*> ABC.many1 (aState False) <?> "node07"
  where
  aNode = (\nty nix -> (nix, Node nty empty 0 0 '-' '-' '-' '-'))
        <$ ABC.skipSpace <* ("[ " <?> "[ ")
        <*> anType <*> (ssN <?> "node ID")
        <* ABC.skipSpace <* "]"

-- | Parse the node type

anType :: ABC.Parser NodeType
anType = ABC.choice [ Bif  <$ "BIF" , MatP <$ "MATP", MatL <$ "MATL", MatR <$ "MATR"
                   , BegL <$ "BEGL", BegR <$ "BEGR", Root <$ "ROOT", End  <$ "END" ] <?> "anType"

-- | Parsing of states.

aState
  :: Bool             -- ^ Control if the four QDB parameters are present. Currently 'node1x' will parse those, 'node07' won't.
  -> ABC.Parser (PInt () StateIndex, State)
aState parseQDB = do
  ABC.skipSpace
  _stateType <- asType <?> "asType"
  stateId    <- ssN
  _stateParents <- (\a b -> VG.fromList [a,b]) <$> ssZ <*> ssN
  (c1,c2)    <- (,) <$> ssZ <*> ssN
  let chdn = if _stateType==B then [ PInt c1 , PInt c2 ]
                              else take c2 [ PInt c1 .. ]
  _stateQDB  <- if parseQDB
                then QDB <$> ssN <*> ssN <*> ssN <*> ssN
                else pure def
  _stateTransitions <- if | _stateType == B  -> pure $ fromList $ map (,0) chdn
                          | otherwise        -> (fromList . zip chdn) <$> ABC.count c2 (Bitscore <$> ssD')
  _stateEmissions   <- if | emitsPair   _stateType -> fromList <$> ABC.count 16 (Bitscore <$> ssD)
                          | emitsSingle _stateType -> fromList <$> ABC.count 4 (Bitscore <$> ssD)
                          | otherwise -> pure empty
  eolS
  return (stateId,State{..})

-- | Type of the state

asType :: ABC.Parser StateType
asType = ABC.choice [ D  <$ "D" , MP <$ "MP", ML <$ "ML", MR <$ "MR"
                   , IL <$ "IL", IR <$ "IR", S  <$ "S" , E  <$ "E" , B <$ "B" ]

-- | Read a list of CMs from a given filename. The special filename @-@
-- reads from @stdin@, while a suffix ending in @.gz@ will pipe through
-- @ungzip@ before parsing the contents.

--fromFile :: FilePath -> IO [CM]
--fromFile fp
--  | fp == "-"                 = runResourceT $ sourceHandle stdin $=           conduitCM $$ consume
--  | takeExtension fp == ".gz" = runResourceT $ sourceFile fp      $= ungzip $= conduitCM $$ consume
--  | otherwise                 = runResourceT $ sourceFile fp      $=           conduitCM $$ consume
--
--test = do
--  cms11 <- fromFile "RF00563.cm"
--  --cms07 <- fromFile "rebecca-kirsch/split_split_chr3L_289_0.maf.gz.fa.cm.h1.3.h2.5"
--  forM_ cms11 $ \cm -> do
--    --print $ cm ^. unknownLines
--    --print $ makeLocal cm
--    --print $ (addLocalEnds cm) ^? nodes . vectorIx 1 . nstates . ix 0 . transitions
--    let q = (addLocalEnds cm) & nodes . vectorIx 1 . nodeMainState EntryState . transitions %~ id
--    mapM_ print $ VG.toList $ q ^. nodes . vectorIx 1 . nstates . ix 0 . transitions
--
