{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Biobase.SElab.Common.Parser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char (isSpace,isAlpha,isDigit)



-- * Helper functions

ssN  = skipSpace *> decimal
ssN_ = skipSpace *> ((-1) <$ "-" <|> decimal)
ssZ  = skipSpace *> signed decimal
ssQ  = skipSpace *> rational
ssD  = skipSpace *> double  <?> "Double"
ssD' = skipSpace *> ((-999999) <$ "*" <|> double) <?> "Double ('*' aware)"
ssS  = skipSpace *> takeTill (\c -> isEndOfLine c || isHorizontalSpace c)
ssC  = skipSpace *> anyChar

{-
ssString = skipSpace *> ABC.takeTill isSpace
ssChar = skipSpace *> ABC.anyChar

xN      = skipSpace *> (Nothing <$ "-" <|> Just <$> decimal)
xString = skipSpace *> (Nothing <$ "-" <|> Just <$> ABC.takeTill isSpace)
xChar   = skipSpace *> (Nothing <$ "-" <|> Just <$> ABC.anyChar)

infoLine = (,) <$> ABC.takeWhile isAlpha <* skipSpace <*> takeTill isEndOfLine <* endOfLine

-}
(..*>) s t = s *> skipSpace *> t

eolS = takeTill isEndOfLine <* endOfLine  -- TODO do we want a version that skips space? probably yes ...
eolB = skipSpace *> (True <$ "yes" <|> False <$ "no") <* endOfLine
eolR = skipSpace *> rational <* endOfLine
eolD = skipSpace *> double <* endOfLine <?> "double, endOfLine"
eolN = skipSpace *> decimal <* endOfLine
eolZ = skipSpace *> signed decimal <* endOfLine

