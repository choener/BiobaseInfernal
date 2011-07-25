{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Enumeratee that transforms a stream of 'ByteString's into a stream of
-- 'VerboseHit's.

module Biobase.Infernal.VerboseHit.Import 
  ( eeToVerboseHit
  ) where

import Control.Applicative
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Class (lift)
import Data.Char (isSpace)
import Data.Tuple.Select
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Char8 as A8
import qualified Data.Attoparsec.Enumerator as EAP
import qualified Data.ByteString.Char8 as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.List as EL

import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Internal



-- | Stateful reading of 'VerboseHit's. This pipe groups certain lines and
-- condenses them to one hit. As multiple hits share certain state (cm,
-- scaffold, and strand), we thread some state through using "goS" internally.
--
-- TODO is MonadIO really necessary?

eeToVerboseHit :: MonadIO m => E.Enumeratee BS.ByteString VerboseHit m b
eeToVerboseHit = goS (AliGo "" "" '?') where
  goS s (E.Continue k) = EL.dropWhile BS.null >> E.peek >>= go s where
    go s Nothing = return $ E.Continue k -- ???
    go s (Just (l :: BS.ByteString))
      | "CM: "    `BS.isInfixOf` l = drops >> E.peek >>= go s{aliCM = BS.copy $ BS.drop 4 l}
      | ">"       `BS.isInfixOf` l = drops >> E.peek >>= go s{aliScaffold = BS.copy $ BS.drop 1 l}
      | "  Plus"  `BS.isInfixOf` l = drops >> E.peek >>= go s{aliStrand = '+'}
      | "  Minus" `BS.isInfixOf` l = drops >> E.peek >>= go s{aliStrand = '-'}
      | " Query"  `BS.isInfixOf` l = do
          x <- qs (aliCM s) (aliScaffold s) (aliStrand s)
          newStep <- lift $ E.runIteratee $ k $ E.Chunks [x]
          goS s newStep
    go s l = drops >> E.peek >>= go s
    drops = EL.drop 1 >> EL.dropWhile BS.null
  goS _ step = return step

-- | Parses one "Alignment". This is build by Query/Score lines and multiple
-- 4-tuples or strings with the actual alignment.

qs :: Monad m => BS.ByteString -> BS.ByteString -> Char -> E.Iteratee BS.ByteString m VerboseHit
qs cm scaf pm = do
  q <- EAP.iterParser qt
  s <- EAP.iterParser sepg
  l <- fourLines $ sel4 q
  return $ VerboseHit
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
    sepg = (,,,) <$ A.string " Score = " <*> A8.double
                 <* A.string ", E = "    <*> A8.double
                 <* A.string ", P = "    <*> A8.double
                 <* A.string ", GC = " <* A8.skipSpace <*> A8.decimal

-- | Parse 4-tuples of cmsearch alignment output until we have hit the last, or
-- "to", 4-tuple. Assumes that "to" is correct

fourLines to = do
  EL.dropWhile BS.null
  ls <- EL.take 4
  let ws = BS.length . BS.takeWhile isSpace . head $ ls
  let cs = BS.length . BS.dropWhile isSpace . head $ ls
  let xs = map (BS.take cs . BS.drop ws) ls
  if (to == (read . BS.unpack . last . BS.words . last $ ls))
  then return . map (:[]) $ xs
  else fourLines to >>= return . (zipWith (:) xs)
