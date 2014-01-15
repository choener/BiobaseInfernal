{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Enumeratee that transforms a stream of 'ByteString's into a stream of
-- 'VerboseHit's.

module Biobase.Infernal.VerboseHit.Import
  ( eneeVerboseHit
  , fromFile
  ) where

import Control.Applicative
import Control.Monad as M
import Data.Attoparsec as A
import Data.Attoparsec.Char8 as A8
import Data.Attoparsec.Iteratee as EAP
import Data.ByteString.Char8 as BS
import Data.Either.Unwrap
import Data.Iteratee as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
import Data.Iteratee.Iteratee as I
import Data.Iteratee.ListLike as I
import Data.Tuple.Select
import Prelude as P

import Biobase.Infernal.Types
import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Transforms a stream into verbose hits. We need to keep a state in the
-- accumulator to keep track of the current CM, scaffold and strand.

eneeVerboseHit :: (Functor m, Monad m) => Enumeratee BS.ByteString [VerboseHit] m a
eneeVerboseHit = enumLinesBS ><> I.filter (not . BS.null) ><> unfoldConvStream f (AliGo BS.empty BS.empty '?' []) where
  f acc = do
    h' <- tryHead
    case h' of
      Nothing -> return (acc, [])
      (Just h)
        | "##"   `isPrefixOf` h -> return (acc{aliAnnotation = aliAnnotation acc ++ [BS.drop 2 h]},[])
        | "CM: " `isPrefixOf` h -> return (acc{aliCM = BS.copy $ BS.drop 4 h, aliAnnotation = []}, [])
        | ">"    `isPrefixOf` h -> return (acc{aliScaffold = BS.copy $ BS.drop 1 h, aliAnnotation = []}, [])
        | "Plus strand results"  `isInfixOf` h -> return (acc{aliStrand = '+', aliAnnotation = []}, [])
        | "Minus strand results" `isInfixOf` h -> return (acc{aliStrand = '-', aliAnnotation = []}, [])
        | " Query" `isInfixOf` h -> do
            x <- qs h (aliCM acc) (aliScaffold acc) (aliStrand acc) (aliAnnotation acc)
            return (acc{aliAnnotation = []},x)
        | otherwise -> return (acc,[])

-- | Parses one CM query result.

qs :: Monad m => ByteString -> ByteString -> ByteString -> Char -> [ByteString] -> Iteratee [ByteString] m [VerboseHit]
qs query cm scaf pm anno = do
  let q = fromRight . parseOnly qt $ query
  s <- I.head >>= return . fromRight . parseOnly sepg
  l <- fourLines $ sel4 q
  return . pure $ VerboseHit
    { vhTarget = Scaffold scaf
    , vhModel = ModelID cm
    , vhStrand = pm
    , vhModelStart = sel1 q
    , vhModelStop = sel2 q
    , vhTargetStart = sel3 q
    , vhTargetStop = sel4 q
    , vhBitScore = BitScore $ sel1 s
    , vhEvalue = sel2 s
    , vhPvalue = sel3 s
    , vhGCpercent = sel4 s
    , vhWuss = cpy $ l!!0
    , vhConsensus = cpy $ l!!1
    , vhScoring = cpy $ l!!2
    , vhSequence = cpy $ l!!3
    , vhAnnotation = anno
    }
  where
    cpy = BS.copy . BS.concat
    qt = (,,,) <$ A.string " Query = "   <*> A8.decimal <* A.string " - " <*> A8.decimal
               <* A.string ", Target = " <*> A8.decimal <* A.string " - " <*> A8.decimal
               <?> "qt"
    sepg = (,,,) <$ A.string " Score = " <*> A8.double
                 <* A.string ", E = "    <*> A8.double
                 <* A.string ", P = "    <*> A8.double
                 <* A.string ", GC = " <* A8.skipSpace <*> A8.decimal
                 <?> "sepg"

-- | Parses multiple four-line elements.

fourLinesOld to = do
  I.dropWhile BS.null
  ls <- joinI $ I.take 4 stream2stream
  let ws = BS.length . BS.takeWhile isSpace . P.head $ ls
  let cs = BS.length . BS.dropWhile isSpace . P.head $ ls
  let xs = P.map (BS.take cs . BS.drop ws) ls
  if (P.length ls /= 4) ||
     ("-" == (P.last . BS.words . P.last $ ls)) ||
     (to == (read . BS.unpack . P.last . BS.words . P.last $ ls))
  then return . P.map (:[]) $ xs
  else fourLines to >>= return . (P.zipWith (:) xs)

fourLines to = do
  I.dropWhile BS.null
  mp <- I.peek
  case mp of
    Nothing -> return $ P.replicate 4 []
    Just p
      | "//" `isInfixOf` p
      || "CM" `isInfixOf` p
      || "Query" `isInfixOf` p
      || ">" `isPrefixOf` p
      || "strand" `isInfixOf` p
      || "#" `isPrefixOf` p
      -> return $ P.replicate 4 []
      | otherwise
      -> do ls <- joinI $ I.take 4 stream2stream
            let ws = BS.length . BS.takeWhile isSpace . P.head $ ls
            let cs = BS.length . BS.dropWhile isSpace . P.head $ ls
            let xs = P.map (BS.take cs . BS.drop ws) ls
            fourLines to >>= return . (P.zipWith (:) xs)

-- | Convenience function: read all results into a single list.

fromFile :: FilePath -> IO [VerboseHit]
fromFile fp = do
  i <- enumFile 8192 fp . joinI $ eneeVerboseHit stream2list
  run i



-- How to use this enumeratee.

{-
test = do
  i <- enumFile 8192 "test.vh" $ joinI $ eneeVerboseHit stream2list
  xs <- run i
  P.mapM_ (\x -> print x >> P.putStrLn "\n\n\n") xs
  print $ P.length xs
-}
