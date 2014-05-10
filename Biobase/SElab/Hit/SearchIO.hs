
module Biobase.SElab.Hit.SearchIO where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.IO.Class (MonadIO)
import           Control.Monad.Trans.Resource (runResourceT,MonadThrow)
import           Data.Attoparsec.ByteString (takeTill,count,many1,(<?>),manyTill,option)
import           Data.ByteString.Char8 (ByteString)
import           Data.Conduit.Attoparsec (conduitParserEither)
import           Data.Conduit.Binary (sourceFile)
import           Data.Conduit.List (consume)
import           Data.Conduit.Text (decodeUtf8)
import           Data.Conduit (yield,awaitForever,(=$=),Conduit,($$),($=))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as AT

import           Biobase.SElab.Hit.Hit



tabularHits :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m Hit
tabularHits = decodeUtf8 =$= conduitParserEither (parseTabular <?> "tabular parser") =$= awaitForever (either (error . show) (yield . snd))

verboseHits :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m Hit
verboseHits = decodeUtf8 =$= conduitParserEither (parseVerbose <?> "verbose parser") =$= awaitForever (either (error . show) (yield . snd))

parseTabular :: AT.Parser Hit
parseTabular = return undefined

parseVerbose :: AT.Parser Hit
parseVerbose = return undefined
