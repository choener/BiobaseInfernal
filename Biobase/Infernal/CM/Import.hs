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
    liftIO $ print ns
    return CM
      { _name          = ID $ hs M.! "NAME"
      , _accession     = AC . readAccession $ hs M.! "ACCESSION"
      , _version       = fromJust infernal10
      , _trustedCutoff = BitScore . readBS $ hs M.! "TC"
      , _gathering     = BitScore . readBS $ hs M.! "GA"
      , _noiseCutoff   = (BitScore . readBS) `fmap` (M.lookup "NC" hs)
      , _nullModel     = VU.fromList . P.map readBS . BS.words $ hs M.! "NULL"

      , _nodes = M.fromList . P.map (\n -> (sel2 n, (sel1 n, undefined))) $ ns

      , _unsorted = M.filter (not . flip P.elem ["NAME","ACCESSION","TC","GA","NC","NULL"]) hs
      }

readBS = read . BS.unpack

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
    _                  -> CL.take 1 >>= \x -> parseStates ntype nid (x:xs)

-- | Determine if a line is a node line ('Just'). If yes, we'll get the node
-- type as string and the node identifier, too.

isNode :: Maybe ByteString -> Maybe (NodeType, NodeID)
isNode (Just xs)
  | BS.null xs = Nothing
  | ["[",ntype,nid,"]"] <- BS.words xs = Just (nodeTypeFromString . BS.unpack $ ntype, NodeID . read . BS.unpack $ nid)
isNode _ = Nothing

{-
data BuildingCM = BuildingCM
  { _cm :: CM
  , _numStates :: Int
  }

$( makeLens ''BuildingCM )

-- * conduit-based parser for human-readable CMs.

parseCM :: (MonadIO m, MonadThrow m) => Conduit ByteString m ByteString
parseCM = C.sequence go where
  go = do
    version <- line
    case version of
      "INFERNAL-1 [1.0]" -> parseCM10 $ BuildingCM {_cm = (CM {_version = version}) }
      -- "INFERNAL-1 [1.1]" -> parseCM11
      _                  -> error $ "can not parse Infernal CM, versioned: " ++ BS.unpack version

-- parseCM10 :: (MonadIO m, MonadThrow m) => 
parseCM10 cm = do
  l <- line
  let (h,t) = second (BS.dropWhile isSpace) . BS.break (==' ') $ l
  case h of
    "NAME"      -> parseCM10 $ (name ^= ID t) cm
    "ACCESSION" -> parseCM10 $ (accession ^= (AC . bsread $ t)) cm
    "GA"        -> parseCM10 $ (gathering ^= (BitScore . bsread $ t)) cm
    "TC"        -> parseCM10 $ (trustedCutoff ^= (BitScore . bsread $ t)) cm
    "NC"        -> parseCM10 $ (noiseCutoff ^= (Just . BitScore . bsread $ t)) cm
--    "//"   -> return $ cm ^. name
    x      -> error $ show (h,t)

-- |

bsread = read . BS.unpack

-- | Get a single line of the input

line = do
  l <- CB.takeWhile (/=10) =$ sinkParser takeByteString
  CB.dropWhile (==10)
  return l
-}

test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "test.cm" $= parseCM10 $$ consume -- sinkHandle stdout
  return () --print xs

-- * iteratee stuff

-- | iteratee-based parsing of human-readable CMs.

{-
eneeCM :: (Monad m) => Enumeratee ByteString [CM] m a
eneeCM = enumLinesBS ><> convStream f where
  f = do
    -- initial (mostly key/value) data
    hs' <- I.takeWhile (/="MODEL:")
    let hs = M.fromList . P.map (second (BS.dropWhile (==' ')) . BS.break (==' ')) $ hs'
    -- model begins
    mb <- I.tryHead
    unless (mb == Just "MODEL:") . error $ "model error: " ++ show (hs,mb,"head")
    -- nodes
    ns <- iterNodes
    -- model ends
    me <- I.tryHead
    unless (me == Just "//") . error $ "model error: " ++ show (hs,me,"tail")
    return . (:[]) $ CM
      { name = ID $ hs M.! "NAME"
      , accession = AC . bsRead . BS.drop 2 $ hs M.! "ACCESSION"
      , gathering = BitScore . bsRead $ hs M.! "GA"
      , trustedCutoff = BitScore . bsRead $ hs M.! "TC"
      , noiseCutoff = let x = hs M.! "NC" in if x == "undefined" then Nothing else Just . BitScore . bsRead $ x
      , transition = error "not implemented yet"
      , emission = error "not implemented yet"
      , paths = error "not implemented yet"
      , localBegin = error "not implemented yet"
      , begins = error "not implemented yet"
      , localEnd = error "not implemented yet"
      , nodes = error "not implemented yet"
      } where bsRead = read . BS.unpack

iterNodes :: (Monad m) => Iteratee [ByteString] m [Node]
iterNodes = do
  hdr' <- I.head
  let (ishdr,(hdr,nidx)) = isNodeHeader hdr'
  unless ishdr $ error $ show hdr'
  xs <- I.takeWhile (fst . isState)
  pk <- I.peek
  let n = Node
            { nodeHeader = hdr
            , nodeIndex = nidx
            }
  case pk of
    Just "//" -> return []
    Just x
      | (True,_) <- isNodeHeader x -> do
          ns <- iterNodes
          return $ n:ns
    e -> error $ show e

data Node = Node
  { nodeHeader :: ByteString
  , nodeIndex :: Int
  }

isNodeHeader :: ByteString -> (Bool,(ByteString,Int))
isNodeHeader xs = (isnh,(hdr,nidx)) where
  isnh = BS.elem '[' xs && BS.elem ']' xs
  [hdr,nidx'] = BS.words . BS.init . BS.takeWhile (/=']') . BS.drop 1 . BS.dropWhile (/='[') $ xs
  nidx = read . BS.unpack $ nidx'

isState :: ByteString -> (Bool,ByteString)
isState xs'
  | P.null xs = (False,"")
  | P.head xs `P.elem` [ "[", "//" ] = (False,"")
  | P.head xs `P.elem` [ "S", "IL", "IR", "MATR", "MR", "D", "MP", "ML", "B", "E" ] = (True,"")
  | otherwise = error $ show xs
  where
    xs = BS.words xs'

-- * convenience functions

-- | Read covariance models from file. This parser reads one or more CMs from
-- file.

fromFile :: FilePath -> IO (ID2CM, AC2CM)
fromFile fp = run =<< ( enumFile 8192 fp
                      . joinI
                      . eneeCM
                      $ I.zip (mkMap name) (mkMap accession)
                      )

-- | Read covariance models from a compressed file.
--
-- TODO currently unusable as iteratee-compress is to old

fromFileZip :: FilePath -> IO (ID2CM, AC2CM)
fromFileZip = undefined
{-
fromFileZip fp = run =<< ( enumFile 8192 fp
                         . joinI
                         . enumInflate GZipOrZlib defaultDecompressParams
                         . joinI
                         . eneeCM
                         $ I.zip (mkMap name) (mkMap accession)
                         )
-}

-- | map creation helper

mkMap f = I.foldl' (\ !m x -> M.insert (f x) x m) M.empty
-}
