{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Parses text-based covariance-model descriptions.

module Biobase.Infernal.CM.Import where

import Control.Arrow
import Control.Monad (unless)
import Data.ByteString.Char8 as BS
import Data.Map as M
import Prelude as P
import Control.Monad.IO.Class (liftIO, MonadIO)

import Data.PrimitiveArray
import Data.PrimitiveArray.Zero

import Biobase.Infernal.CM
import Biobase.Infernal.Types



-- * conduit-based parser for human-readable CMs.




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