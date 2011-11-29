{-# LANGUAGE OverloadedStrings #-}

-- Importing tabular hits is rather easy, as they are one entry per line.

module Biobase.Infernal.TabularHit.Import where

import Data.ByteString.Char8 as BS
import Data.Iteratee as I
import Data.Iteratee.Iteratee as I
import Data.Iteratee.ListLike as I
import Data.Iteratee.Char as I
import Data.Either.Unwrap
import Data.Attoparsec as A hiding (takeTill)
import Data.Attoparsec.Char8 as A
import Control.Applicative
import Data.Iteratee.IO as I

import Biobase.Infernal.TabularHit



-- | Transform a stream into tabular hits.

eneeTabularHit :: (Functor m, Monad m) => Enumeratee ByteString [TabularHit] m a
eneeTabularHit = enumLinesBS ><> I.filter (\x -> not $ BS.null x || isPrefixOf "#" x) ><> mapStream f where
  f = fromRight . parseOnly p
  p = TabularHit <$> pString -- model name
                 <*> pString -- target name
                 <*> pDecimal -- target start
                 <*> pDecimal -- target stop
                 <*> pDecimal -- query start
                 <*> pDecimal -- query stop
                 <*> pDouble -- bit score
                 <*> pDouble -- evalue
                 <*> pDecimal -- gc content
  pString  = A.skipSpace *> A.takeTill A.isSpace
  pDecimal = A.skipSpace *> A.decimal
  pDouble  = A.skipSpace *> A.double

-- | Convenience function to load from file and return a big list of tabular
-- hits.

fromFile :: FilePath -> IO [TabularHit]
fromFile fp = do
  i <- enumFile 8192 fp . joinI $ eneeTabularHit stream2stream
  run i
