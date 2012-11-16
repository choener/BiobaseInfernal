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

import Control.Applicative
import Control.Arrow
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad (unless)
import Data.Attoparsec.ByteString as AB
import Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Double as BS
import Data.Char (isSpace,isAlpha,isDigit)
import Data.Conduit as C
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Map as M
import Data.Maybe as M
import Data.Tuple.Select
import Data.Vector.Unboxed as VU (fromList)
import Prelude as P
import System.IO (stdout)

import Data.PrimitiveArray
import Data.PrimitiveArray.Zero

import Biobase.SElab.CM
import Biobase.SElab.Types
import qualified Biobase.SElab.HMM as HMM
import qualified Biobase.SElab.HMM.Import as HMM



-- * Covariance model parsing.

-- ** Infernal 1.0 and 1.1 covariance model parser

parseHeader = ($) <$ AB.string "INFERNAL" *> (Infernal10 <$ AB.string "-1" <|> Infernal11 <$ AB.string "1/a") <*> AB.takeByteString <?> "INFERNAL line"

lineParser p = CL.head >>= \x -> return . maybe (error "no more input") (either (\e -> error $ show (e,x)) id . AB.parseOnly p) $ x

-- | Top-level parser for Infernal 1.0 and 1.1 human-readable covariance
-- models. Reads all lines first, then builds up the CM.

parseCM1x :: (Monad m, MonadIO m) => Conduit ByteString m CM
parseCM1x = CB.lines =$= CL.sequence go where
  go = do
    hdr <- lineParser parseHeader
    hs <- parseHeaders []
    ns <- parseNodes hdr []
    let nsMap = M.fromList . P.map (\n -> (sel2 n, (sel1 n, P.map (^. stateID) $ sel3 n))) $ ns
    let ssMap = M.fromList . P.map ((^. stateID) &&& id) .  P.concatMap (sel3) $ ns
    lineParser $ (AB.string "//" <?> "model end")
    pk <- CL.peek
    hmm <- case HMM.legalHMM pk of
      True -> Just `fmap` HMM.parseHMM3
      False -> return Nothing
    return CM
      { _name          = IDD $ hs M.! "NAME"
      , _accession     = ACC . readAccession . P.head . M.catMaybes $ P.map (`M.lookup` hs) ["ACC", "ACCESSION"]
      , _version       = hdr
      , _trustedCutoff = BitScore . readBS $ hs M.! "TC"
      , _gathering     = BitScore . readBS $ hs M.! "GA"
      , _noiseCutoff   = (BitScore . readBS) `fmap` (M.lookup "NC" hs)
      , _nullModel     = VU.fromList . P.map readBitScore . BS.words $ hs M.! "NULL"

      , _nodes  = nsMap
      , _states = ssMap

      , _localBegin = flip M.singleton (BitScore 0) . (^.stateID) . P.head . P.filter (\s -> s^.stateType == S && s^.nodeID == NodeID 0 ) . M.elems $ ssMap
      , _localEnd = M.empty

      , _unsorted = M.filter (not . flip P.elem ["NAME","ACCESSION","TC","GA","NC","NULL"]) hs
      , _hmm = hmm
      }

readBS = read . BS.unpack
readBitScore "*" = BitScore $ -1/0
readBitScore x = BitScore . readBS $ x

readAccession xs
  | BS.length xs /= 7 = error $ "can't read accession: " ++ BS.unpack xs
  | "RF" == hdr && P.all isDigit tl = read tl
  | otherwise = error $ "readAccession: " ++ BS.unpack xs
  where (hdr,tl) = second BS.unpack . BS.splitAt 2 $ xs

-- | Infernal 1.0 header parser. Greps all lines until the "MODEL:" line, then
-- return lines to top-level parser. Parses three lines at once in case of
-- "FT-" lines.

parseHeaders hs = do
  p <- CL.head
  case p of
    (finishedHeader -> True) -> return . M.fromList 
                                       . P.map (second (BS.dropWhile isSpace)
                                       . BS.break isSpace)
                                       . P.reverse
                                       $ hs
    Nothing -> error $ "unexpected end of header, until here:" ++ (show $ P.reverse hs)
    Just "" -> error "empty line"
    Just l  -> do ls <- if ("FT-" `isPrefixOf` l) then CL.take 2 else return []
                  let lls = BS.concat $ l:ls
                  parseHeaders (lls:hs)

finishedHeader :: Maybe ByteString -> Bool
finishedHeader (Just x) = go x where
  go "MODEL:" = True
  go "CM" = True
  go _ = False
finishedHeader _ = False

-- | Parses nodes. Will terminate on "//" which ends a CM. The state parser
-- will just peek on "//", not remove it from the stream.
--
-- A node is (node type, node id, set of states)

parseNodes hdr ns = do
  p <- CL.peek
  case (BS.dropWhile isAlpha `fmap` p) of
    Nothing -> error "unexpected empty line"
    Just "//" -> return . P.reverse $ ns
    (isNode -> Just (ntype,nid)) -> do _ <- CL.head -- kill the line
                                       ss <- parseStates hdr ntype nid []
                                       parseNodes hdr $ (ntype,nid,ss):ns

-- | Parses all states for a node. We peek at the first line, then handle
-- accordingly: if "//" the model will be done; is a node is coming up, return
-- the state lines read until now.

parseStates hdr ntype nid xs = do
  p <- CL.peek
  case (BS.dropWhile isSpace `fmap` p) of
    Nothing -> error "unexpected empty state"
    Just "//" -> return . P.reverse $ xs
    (isNode -> Just _) -> return . P.reverse $ xs
    _                  -> do Just x <- CL.head
                             let psx = parseState hdr ntype nid x
                             parseStates hdr ntype nid (psx:xs)

-- parseState :: ByteString -> State
parseState hdr ntype nid s
  | P.null ws = error "parseState: no words"
  | B == t    = State { _stateID = StateID . readBS $ pn!!0
                      , _stateType = t
                      , _nodeID = nid
                      , _nodeType = ntype
                      , _transitions = [ ( StateID . readBS $ pn!!3, 0)
                                       , ( StateID . readBS $ pn!!4, 0)
                                       ]
                      , _emits = EmitNothing
                      }
  | otherwise = State { _stateID = StateID . readBS $ pn!!0
                      , _stateType = t
                      , _nodeID = nid
                      , _nodeType = ntype
                      , _transitions = [ (StateID (i+k), readBitScore $ ts!!k) | k <- [0..n-1]]
                      , _emits = e
                      }
  where
    ws = BS.words s
    numPN = case hdr of
      Infernal10 _ -> 5
      Infernal11 _ -> 9 -- last 4 values are QDB values ...
    numTS = readBS $ pn!!4
    numES = case w of
              "MP" -> 16
              (flip P.elem ["ML","MR","IL","IR"] -> True) -> 4
              _    -> 0
    ~([w],~(pn,~(ts,es))) = second (second (second (P.map readBitScore) . P.splitAt numTS) . P.splitAt numPN) . P.splitAt 1 $ ws
    t = readBS w :: StateType
    i = readBS $ pn!!3
    n = readBS $ pn!!4
    e = case t of
          MP -> EmitsPair $ P.zipWith (\(c1,c2) k -> (c1,c2,k)) [ (c1,c2) | c1 <- "ACGU", c2 <- "ACGU" ] es
          ((flip P.elem [ML,MR,IL,IR]) -> True) -> EmitsSingle $ P.zip "ACGU" es
          _ -> EmitNothing



{-
parseState hdr ntype nid s
  | B == t = State { _stateID = StateID . readBS $ ws!!1
                   , _stateType = B
                   , _nodeID = nid
                   , _nodeType = ntype
                   , _transitions = [ ( StateID . readBS $ ws!!4, 0)
                                    , ( StateID . readBS $ ws!!5, 0)
                                    ]
                   , _emits = EmitNothing
                   }
  | otherwise = State { _stateID = StateID . readBS $ ws!!1
                      , _stateType = t -- stateTypeFromString . BS.unpack $ t
                      , _nodeID = nid
                      , _nodeType = ntype
                      , _transitions = [ (StateID (i+k), readBitScore $ ws!!(6+k))
                                       | k <- [0 .. n-1] ]
                      , _emits = e
                      }
  where
    last k = P.map readBitScore . P.reverse . P.take k . P.reverse $ ws
    (t':_) = ws
    n = readBS $ ws!!5 -- number of states
    i = readBS $ ws!!4 -- first state
    e = case t of
          MP -> EmitsPair $ P.zipWith (\(c1,c2) k -> (c1,c2,k)) [ (c1,c2) | c1 <- "ACGU", c2 <- "ACGU" ] (last 16)
          ((flip P.elem [ML,MR,IL,IR]) -> True) -> EmitsSingle . P.zip "ACGU" $ last 4
          _ -> EmitNothing
-}

-- | Determine if a line is a node line ('Just'). If yes, we'll get the node
-- type as string and the node identifier, too.

isNode :: Maybe ByteString -> Maybe (NodeType, NodeID)
isNode (Just xs)
  | BS.null xs = Nothing
  | ("[":ntype:nid:"]":cm11) <- BS.words xs = Just (readBS ntype, NodeID . readBS $ nid)
isNode _ = Nothing

fromFile :: FilePath -> IO [CM]
fromFile fp = do
  runResourceT $ sourceFile fp $= parseCM1x $$ consume

test :: IO ()
test = do
  xs10 <- runResourceT $ sourceFile "test10.cm" $= parseCM1x $$ consume -- sinkHandle stdout
  xs11 <- runResourceT $ sourceFile "test11.cm" $= parseCM1x $$ consume -- sinkHandle stdout
  print xs10
  print xs11
  return ()

