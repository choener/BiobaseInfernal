{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Common parser helpers. The @-S@ versions convert from @ByteString@ to
-- @Text@ here.

module Biobase.SElab.Common.Parser where

import Control.Applicative
import Data.Attoparsec.ByteString.Char8
import Data.Char (isAlpha,isDigit)
import Data.Char.Util
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (takeWhile)



-- * Helper functions

ssN  = skipSpace *> decimal
ssN_ = skipSpace *> ((-1) <$ "-" <|> decimal)
ssZ  = skipSpace *> signed decimal
ssQ  = skipSpace *> rational
ssD  = skipSpace *> double  <?> "Double"
ssD' = skipSpace *> ((-999999) <$ "*" <|> double) <?> "Double ('*' aware)"
ssS  = skipSpace *> takeTill (\c' -> let c = c2w8 c' in isEndOfLine c || isHorizontalSpace c)
ssT  = decodeUtf8 <$ skipSpace <*> takeTill (\c' -> let c = c2w8 c' in isEndOfLine c || isHorizontalSpace c)
ssC  = skipSpace *> anyChar

eolD = skipSpace *> double         <* endOfLine <?> "eolD"
eolN = skipSpace *> decimal        <* endOfLine <?> "eolN"
eolR = skipSpace *> rational       <* endOfLine <?> "eolR"
eolZ = skipSpace *> signed decimal <* endOfLine <?> "eolZ"
eolC = skipSpace *> satisfy (not . isSpace)           <* endOfLine <?> "eolC"
eolS = takeWhile (isHorizontalSpace . c2w8) *> takeTill (isEndOfLine . c2w8) <* endOfLine <?> "eolS"
eolT = decodeUtf8 <$ takeWhile (isHorizontalSpace . c2w8) <*> takeTill (isEndOfLine . c2w8) <* endOfLine <?> "eolS"
eolB = skipSpace *> (True <$ "yes" <|> False <$ "no") <* endOfLine <?> "eolB"

