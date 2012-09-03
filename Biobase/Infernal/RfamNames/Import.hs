{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

module Biobase.Infernal.RfamNames.Import where

import Control.Lens
import Data.Conduit as C
import Data.Conduit.Attoparsec
import Data.Conduit.Binary as CB
import Data.Conduit.List as CL
import Data.Conduit.Util as C
import Data.Either.Unwrap as E
import Data.Attoparsec as A hiding (parse)
import qualified Data.Map as M
import Control.Applicative
import Data.Attoparsec.Char8 as A8 hiding (parse)
import qualified Data.ByteString.Char8 as BS
import Data.Map (Map)

import Biobase.Infernal.RfamNames
import Biobase.Infernal.Types

import Debug.Trace


parse =  CB.lines
      =$ CL.map (parseOnly mkRfamName)
      =$ CL.filter isRight
      =$ CL.map fromRight
      =$ C.zipSinks mapIdRfamNames mapAcRfamNames
{-# INLINE parse #-}

mkRfamName = f <$> rfamAC <* char ';' <*> rfamID <* char ';' <*> seqident <* spaces <*> specAC <* char ':' <*> specID where
  f rfac rfid sid spac spid = ModelNames rfac rfid spac spid
  rfamAC = AC <$ string "RF" <*> decimal
  rfamID = ID <$> A8.takeTill (==';')
  seqident = A8.takeTill isSpace
  specAC = (fmap (AC . read . BS.unpack) . maybeBS) <$> A8.takeTill (==':')
  specID = (fmap ID . maybeBS) <$> takeByteString
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
