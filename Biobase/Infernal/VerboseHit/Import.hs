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

import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Transforms a stream into verbose hits. We need to keep a state in the
-- accumulator to keep track of the current CM, scaffold and strand.

eneeVerboseHit :: (Functor m, Monad m) => Enumeratee BS.ByteString [VerboseHit] m a
eneeVerboseHit = enumLinesBS ><> I.filter (not . BS.null) ><> unfoldConvStream f (AliGo BS.empty BS.empty '?') where
  f acc = do
    h' <- tryHead
    case h' of
      Nothing -> return (acc, [])
      (Just h)
        | "CM: "    `BS.isInfixOf` h -> return (acc{aliCM = BS.copy $ BS.drop 4 h}, [])
        | ">"       `BS.isInfixOf` h -> return (acc{aliScaffold = BS.copy $ BS.drop 1 h}, [])
        | "  Plus"  `BS.isInfixOf` h -> return (acc{aliStrand = '+'}, [])
        | "  Minus" `BS.isInfixOf` h -> return (acc{aliStrand = '-'}, [])
        | " Query"  `BS.isInfixOf` h -> do
            x <- qs h (aliCM acc) (aliScaffold acc) (aliStrand acc)
            return (acc,x)
        | otherwise -> return (acc,[])

-- | Parses one CM query result.

qs :: Monad m => BS.ByteString -> BS.ByteString -> BS.ByteString -> Char -> Iteratee [BS.ByteString] m [VerboseHit]
qs query cm scaf pm = do
  let q = fromRight . parseOnly qt $ query
  s <- I.head >>= return . fromRight . parseOnly sepg
  l <- fourLines $ sel4 q
  return . pure $ VerboseHit
    { vhScaffold = scaf
    , vhCM = cm
    , vhStrand = pm
    , vhQuery = (sel1 q, sel2 q)
    , vhTarget = (sel3 q, sel4 q)
    , vhScore = sel1 s
    , vhEvalue = sel2 s
    , vhPvalue = sel3 s
    , vhGC = sel4 s
    , vhWuss = cpy $ l!!0
    , vhConsensus = cpy $ l!!1
    , vhScoring = cpy $ l!!2
    , vhSequence = cpy $ l!!3
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

fourLines to = do
  I.dropWhile BS.null
  ls <- joinI $ I.take 4 stream2stream
  let ws = BS.length . BS.takeWhile isSpace . P.head $ ls
  let cs = BS.length . BS.dropWhile isSpace . P.head $ ls
  let xs = P.map (BS.take cs . BS.drop ws) ls
  if (to == (read . BS.unpack . P.last . BS.words . P.last $ ls))
  then return . P.map (:[]) $ xs
  else fourLines to >>= return . (P.zipWith (:) xs)

-- | Convenience function: read all results into a single list.

fromFile :: FilePath -> IO [VerboseHit]
fromFile fp = do
  i <- enumFile 8192 fp . joinI $ eneeVerboseHit stream2list
  run i

{- How to use this enumeratee.


test = do
  i <- enumFile 8192 "/home/choener/tmp/infernal-1.0.2/tutorial/tmp.res" $ joinI $ eneeVerboseHit stream2list
  xs <- run i
  print xs
  print $ P.length xs
-}
