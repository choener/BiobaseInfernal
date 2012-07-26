{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses text-based covariance-model descriptions.

module Biobase.Infernal.CM.Import where

import Control.Arrow
import Control.Monad (unless)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Double as BS
import Data.Map as M
import Prelude as P
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.PrimitiveArray
import Data.PrimitiveArray.Zero

import Biobase.Infernal.CM
import Biobase.Infernal.Types

import Data.Conduit as C
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Attoparsec
import Data.Attoparsec.ByteString as AB
import System.IO (stdout)

import Data.Lens.Common
import Data.Lens.Template
import Data.Char (isSpace,isAlpha,isDigit)
import Data.Maybe (fromJust)
import Data.Vector.Unboxed as VU (fromList)
import Data.Tuple.Select



-- * Covariance model parsing.

-- ** Infernal 1.0 covariance model parser

-- | Top-level parser for Infernal 1.0 human-readable covariance models. Reads
-- all lines first, then builds up the CM.

parseCM10 :: (Monad m, MonadIO m) => Conduit ByteString m CM
parseCM10 = CB.lines =$= CL.sequence go where
  go = do
    infernal10 <- CL.head
    unless (infernal10 == Just "INFERNAL-1 [1.0]") . error $ "unexpected, no CM start at: " ++ show infernal10
    hs <- parseHeaders []
    ns <- parseNodes  []
    return CM
      { _name          = ID $ hs M.! "NAME"
      , _accession     = AC . readAccession $ hs M.! "ACCESSION"
      , _version       = fromJust infernal10
      , _trustedCutoff = BitScore . readBS $ hs M.! "TC"
      , _gathering     = BitScore . readBS $ hs M.! "GA"
      , _noiseCutoff   = (BitScore . readBS) `fmap` (M.lookup "NC" hs)
      , _nullModel     = VU.fromList . P.map readBitScore . BS.words $ hs M.! "NULL"

      , _nodes = M.fromList . P.map (\n -> (sel2 n, (sel1 n, P.map (^. stateID) $ sel3 n))) $ ns
      , _states = M.fromList . P.map ((^. stateID) &&& id) .  P.concatMap (sel3) $ ns

      , _unsorted = M.filter (not . flip P.elem ["NAME","ACCESSION","TC","GA","NC","NULL"]) hs
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
    Nothing -> error $ "unexpected end of header, until here:" -- ++ show hs
    Just "MODEL:" -> return . M.fromList . P.map (second (BS.dropWhile isSpace) . BS.break isSpace) . P.reverse $ hs
    Just "" -> error "empty line"
    Just l  -> do ls <- if ("FT-" `isPrefixOf` l) then CL.take 2 else return []
                  let lls = BS.concat $ l:ls
                  parseHeaders (lls:hs)

-- | Parses nodes. Will terminate on "//" which ends a CM. The state parser
-- will just peek on "//", not remove it from the stream.
--
-- A node is (node type, node id, set of states)

parseNodes ns = do
  p <- CL.head
  case (BS.dropWhile isAlpha `fmap` p) of
    Nothing -> error "unexpected empty line"
    Just "//" -> return . P.reverse $ ns
    (isNode -> Just (ntype,nid)) -> do ss <- parseStates ntype nid []
                                       parseNodes $ (ntype,nid,ss):ns

-- | Parses all states for a node. We peek at the first line, then handle
-- accordingly: if "//" the model will be done; is a node is coming up, return
-- the state lines read until now.

parseStates ntype nid xs = do
  p <- CL.peek
  case (BS.dropWhile isSpace `fmap` p) of
    Nothing -> error "unexpected empty state"
    Just "//" -> return . P.reverse $ xs
    (isNode -> Just _) -> return . P.reverse $ xs
    _                  -> CL.take 1 >>= \[x] -> parseStates ntype nid (parseState x:xs)

parseState :: ByteString -> State
parseState s
  | P.null ws = error "parseState: no words"
  | B == t && P.length ws == 6 = State { _stateID = StateID . readBS $ ws!!1
                                         , _stateType = B
                                         , _transitions = [ ( StateID . readBS $ ws!!4, 0)
                                                          , ( StateID . readBS $ ws!!5, 0)
                                                          ]
                                         , _emits = EmitNothing
                                         }
  | otherwise = State { _stateID = StateID . readBS $ ws!!1
                      , _stateType = t -- stateTypeFromString . BS.unpack $ t
                      , _transitions = [ (StateID (i+k), readBitScore $ ws!!(6+k))
                                       | k <- [0 .. n-1] ]
                      , _emits = e
                      }
  where
    last k = P.map readBitScore . P.reverse . P.take k . P.reverse $ ws
    ws = BS.words s
    (t':_) = ws
    t = readBS t' :: StateType
    n = readBS $ ws!!5 -- number of states
    i = readBS $ ws!!4 -- first state
    e = case t of
          MP -> EmitsPair . P.zip [ (c1,c2) | c1 <- "ACGU", c2 <- "ACGU" ] $ last 16
          ((flip P.elem [ML,MR,IL,IR]) -> True) -> EmitsSingle . P.zip "ACGU" $ last 4
          _ -> EmitNothing

-- | Determine if a line is a node line ('Just'). If yes, we'll get the node
-- type as string and the node identifier, too.

isNode :: Maybe ByteString -> Maybe (NodeType, NodeID)
isNode (Just xs)
  | BS.null xs = Nothing
  | ["[",ntype,nid,"]"] <- BS.words xs = Just (readBS ntype, NodeID . readBS $ nid)
isNode _ = Nothing



test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "test.cm" $= parseCM10 $$ consume -- sinkHandle stdout
  print xs

