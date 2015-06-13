{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}

module Biobase.SElab.Common.Parser where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Char (isSpace,isAlpha,isDigit)
import           Prelude hiding (takeWhile)



-- * Helper functions

ssN  = skipSpace *> decimal
ssN_ = skipSpace *> ((-1) <$ "-" <|> decimal)
ssZ  = skipSpace *> signed decimal
ssQ  = skipSpace *> rational
ssD  = skipSpace *> double  <?> "Double"
ssD' = skipSpace *> ((-999999) <$ "*" <|> double) <?> "Double ('*' aware)"
ssS  = skipSpace *> takeTill (\c -> isEndOfLine c || isHorizontalSpace c)
ssC  = skipSpace *> anyChar

eolD = skipSpace *> double         <* endOfLine <?> "eolD"
eolN = skipSpace *> decimal        <* endOfLine <?> "eolN"
eolR = skipSpace *> rational       <* endOfLine <?> "eolR"
eolZ = skipSpace *> signed decimal <* endOfLine <?> "eolZ"
eolC = skipSpace *> satisfy (not . isSpace)           <* endOfLine <?> "eolC"
eolS = takeWhile isHorizontalSpace *> takeTill isEndOfLine              <* endOfLine <?> "eolS"
eolB = skipSpace *> (True <$ "yes" <|> False <$ "no") <* endOfLine <?> "eolB"

