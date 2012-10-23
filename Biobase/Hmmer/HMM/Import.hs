{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Import HMMER3 HMM models.

module Biobase.Hmmer.HMM.Import where

import Control.Monad.IO.Class (liftIO, MonadIO)
import Data.ByteString.Char8 as BS
import Data.ByteString.Lex.Double as BS
import Data.Conduit as C
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Control.Monad (unless)
import Prelude as P
import Control.Arrow
import qualified Data.Map as M
import Data.Char (toLower)

import Biobase.Hmmer.HMM
import Biobase.Infernal.Types



-- * Different HMMer parsers

-- ** HMMER3 / b

-- |
--
-- TODO not everything is currently being parsed. Notably the rf,cs,alignmap
-- annotations.

parseHMM3 :: (Monad m, MonadIO m) => Conduit ByteString m HMM3
parseHMM3 = CB.lines =$= CL.sequence go where
  go = do
    hdr' <- CL.head
    unless (legalHMM hdr') . error $ "no legal HMM at header: " ++ show hdr'
    let Just hdr = hdr'
    hs <- headerMap `fmap` headerLines
    (sas,ths) <- sathLines
    let asize = P.length sas
    c <- compoLine
    n0 <- parseBegin asize
    ns <- parseNodes asize
    Just "//" <- CL.head
    return $ HMM3
      { _version = second (BS.dropWhile (==' ')) . BS.span (/=' ') $ hdr
      , _idd = IDD $ hs M.! "NAME"
      , _acc = fmap (ACC . readBS) $ "AC" `M.lookup` hs
      , _description = "DESC" `M.lookup` hs
      , _leng = readBS $ hs M.! "LENG"
      , _alph = readAlph $ hs M.! "ALPH"
      , _rf = readBoolean $ M.findWithDefault "no" "RF" hs
      , _cs = readBoolean $ M.findWithDefault "no" "CS" hs
      , _alignMap = readBoolean $ M.findWithDefault "no" "MAP" hs
      , _date = M.findWithDefault "" "DATE" hs
      , _symAlph = sas
      , _transHeaders = ths
      , _compo = c
      , _nodes = n0:ns
      }

-- | Check, if we have a legal HMMER3 model.

legalHMM :: Maybe ByteString -> Bool
legalHMM (Just s)
  | w == "HMMER3/f" = True
  | w == "HMMER3/b"   = True
  where (w:_) = BS.words s
legalHMM _ = False



-- * Helper functions

-- | Read boolean flags.

readBoolean = f . BS.map toLower where
  f "no" = False
  f "yes" = True
  f x = error $ "unknown boolean: " ++ show x

-- | Determine which alphabet is in use by the HMM.

readAlph = f . BS.map toLower where
  f "dna"    = DNA
  f "rna"    = RNA
  f "coins"  = Coins
  f "dice"   = Dice
  f "amino"  = Amino
  f "custom" = Custom
  f a        = error $ "unknown alph: " ++ show a

-- | Read from a bytestring into a structure.

readBS = read . BS.unpack

-- | create associative map of the key/value data.

headerMap xs = M.fromList . P.map f $ xs where
  f = second (BS.dropWhile (==' ')) . BS.span (/=' ')

-- | Parse the two beginning lines.

parseBegin asize = do
  Just i' <- CL.head
  Just t' <- CL.head
  return $ Node
            0
            []
            (P.map (readNLP . BS.unpack) $ BS.words i')
            (P.map (readNLP . BS.unpack) $ BS.words t')

-- | Parse all individual nodes, except the first one, which uses 'parseBegin'.

parseNodes asize = go [] where
  go xs = do
    p <- CL.peek
    case p of
      (Just "//") -> return $ P.reverse xs
      _ -> do Just m' <- CL.head
              Just i' <- CL.head
              Just t' <- CL.head
              let (nid:m) = BS.words m'
              let n = Node
                        (read . BS.unpack $ nid)
                        (P.map (readNLP . BS.unpack) $ P.take asize m)
                        (P.map (readNLP . BS.unpack) $ BS.words i')
                        (P.map (readNLP . BS.unpack) $ BS.words t')
              go (n:xs)

-- | Read a HMMER negated log-probability.

readNLP :: String -> NegLogProb
readNLP = go where
  go "*" = NLP $ 1/0
  go xs  = NLP . read $ xs

-- | Read the optional COMPO line.

compoLine = do
  Just p <- CL.peek
  case (BS.words p) of
    ("COMPO":xs) -> CL.head >>= \_ -> return $ P.map (NLP . read . BS.unpack) xs
    _ -> return []

-- | Read the alphabet and transition lines.

sathLines = do
  Just sa' <- CL.head
  Just th' <- CL.head
  let (sa:sas) = BS.words sa'
  let ths = BS.words th'
  if sa == "HMM"
  then return (sas,ths)
  else error $ "NOT THE HMM symalph lines: " ++ show (sa:sas,ths)

-- | All the header lines until we see "HMM".

headerLines = go [] where
  go xs = do
    p <- CL.peek
    case p of
      (Just x) | "HMM" `BS.isPrefixOf` x -> return $ P.reverse xs
               | otherwise -> CL.drop 1 >>  go (x:xs)
      Nothing -> error $ "no more lines after: " ++ show (P.reverse xs)



-- | Simple test for the HMMer parser.

test :: IO ()
test = do
  xs <- runResourceT $ sourceFile "test.hmm" $= parseHMM3 $$ consume -- sinkHandle stdout
  print xs

