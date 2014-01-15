{-# LANGUAGE OverloadedStrings #-}

module Biobase.SElab.Common.Parser where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8 (endOfLine,skipSpace,decimal,double,rational,isEndOfLine,(.*>),signed)
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.ByteString.Char8 as ABC
import           Data.Char (isSpace,isAlpha,isDigit)



-- * Helper functions

ssN = skipSpace *> decimal
ssZ = skipSpace *> signed decimal
ssQ = skipSpace *> rational
ssD = skipSpace *> double
ssString = skipSpace *> ABC.takeTill isSpace
ssChar = skipSpace *> ABC.anyChar

xN      = skipSpace *> (Nothing <$ "-" <|> Just <$> decimal)
xString = skipSpace *> (Nothing <$ "-" <|> Just <$> ABC.takeTill isSpace)
xChar   = skipSpace *> (Nothing <$ "-" <|> Just <$> ABC.anyChar)

infoLine = (,) <$> ABC.takeWhile isAlpha <* skipSpace <*> takeTill isEndOfLine <* endOfLine

(..*>) s t = s .*> skipSpace *> t

eolS = takeTill isEndOfLine <* endOfLine  -- TODO do we want a version that skips space? probably yes ...
eolR = skipSpace *> rational <* endOfLine
eolD = skipSpace *> double <* endOfLine
eolN = skipSpace *> decimal <* endOfLine
eolZ = skipSpace *> signed decimal <* endOfLine
eolB = skipSpace *> (True <$ "yes" <|> False <$ "no") <* endOfLine

