{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Biobase.SElab.RfamNames.Import where

import Control.Applicative
import Control.Lens
import Data.Attoparsec as A hiding (parse)
import Data.Attoparsec.Char8 as A8 hiding (parse)
import Data.Conduit as C
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Util as C
import Data.Either.Unwrap as E
import Data.Map (Map)
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M

import Biobase.SElab.RfamNames
import Biobase.SElab.Types



parse =  CB.lines
      =$ CL.map (parseOnly mkRfamName)
      =$ CL.filter isRight
      =$ CL.map fromRight
      =$ C.zipSinks mapIdRfamNames mapAcRfamNames
{-# INLINE parse #-}

mkRfamName = f <$> rfamAC <* char ';' <*> rfamID <* char ';' <*> seqident <* spaces <*> specAC <* char ':' <*> specID where
  f rfac rfid sid spac spid = ModelNames rfac rfid spac spid
  rfamAC = ACC <$ string "RF" <*> decimal
  rfamID = IDD <$> A8.takeTill (==';')
  seqident = A8.takeTill isSpace
  specAC = (fmap (ACC . read . BS.unpack) . maybeBS) <$> A8.takeTill (==':')
  specID = (fmap IDD . maybeBS) <$> takeByteString
  spaces = many1 space
  maybeBS s
    | BS.null s = Nothing
    | otherwise = Just s
{-# INLINE mkRfamName #-}

mapIdRfamNames = CL.fold f M.empty where
  f !mp x = M.insertWith' (++) (x ^. modelID) [x] mp
{-# INLINE mapIdRfamNames #-}

mapAcRfamNames = CL.fold f M.empty where
  f !mp x = M.insertWith' (++) (x ^. modelAC) [x] mp
{-# INLINE mapAcRfamNames #-}

fromFile :: String -> IO ( Map (Identification Rfam) [ModelNames]
                         , Map (Accession      Rfam) [ModelNames]
                         )
fromFile fname = do
  runResourceT $ CB.sourceFile fname $$ parse
{-# NOINLINE fromFile #-}
