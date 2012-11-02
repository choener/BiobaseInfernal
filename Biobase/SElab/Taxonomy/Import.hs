{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE BangPatterns #-}

-- | Iteratee-based importer. Provides a simple "fromFile" function that
-- produces both maps in one pass.

module Biobase.SElab.Taxonomy.Import where

import Control.Applicative
import Control.Lens
import Data.Attoparsec as A hiding (parse)
import Data.Attoparsec.Char8 (char,decimal)
import Data.ByteString.Char8 as BS
import Data.Conduit as C
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Util as C
import Data.Either.Unwrap as E
import Data.List as L
import Data.Map as M
import qualified Data.Attoparsec.ByteString as AB hiding (parse)
import qualified Data.Attoparsec.Char8 as A8

import Biobase.SElab.Taxonomy
import Biobase.SElab.Types



parse =  CB.lines
      =$ CL.map (parseOnly mkTaxonomy)
      =$ CL.filter isRight
      =$ CL.map fromRight
      =$ C.zipSinks mapIdTaxonomy mapAcTaxonomy
{-# INLINE parse #-}

mkTaxonomy :: Parser Taxonomy
mkTaxonomy = f <$> ptaxid <* tab <*> pname <* tab <*> takeByteString where
  f k n xs = let
               cs = L.map (Classification . copy . BS.dropWhile (==' ')) . BS.split ';' . BS.init $ xs
             in Taxonomy (ACC k) (IDD $ copy n) cs
  ptaxid   = decimal
  pname    = A8.takeWhile (/='\t')
  tab      = char '\t'
{-# INLINE mkTaxonomy #-}

mapIdTaxonomy :: Monad m => GSink Taxonomy m (M.Map (Identification Species) Taxonomy)
mapIdTaxonomy = CL.fold f M.empty where
  f !mp x = M.insert (x ^. name) x mp
{-# INLINE mapIdTaxonomy #-}

mapAcTaxonomy :: Monad m => GSink Taxonomy m (M.Map (Accession Species) Taxonomy)
mapAcTaxonomy = CL.fold f M.empty where
  f !mp x = M.insert (x ^. accession) x mp
{-# INLINE mapAcTaxonomy #-}

fromFile :: String -> IO ( Map (Identification Species) Taxonomy
                         , Map (Accession Species) Taxonomy
                         )
fromFile fname = do
  runResourceT $ CB.sourceFile fname $$ parse
{-# NOINLINE fromFile #-}
