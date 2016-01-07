{-# LANGUAGE TemplateHaskell #-}

module Biobase.SElab.Hit.SearchIO where

import           Control.Lens
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



tabularHits :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m HitStream
tabularHits = decodeUtf8 =$= conduitParserEither (parseTabular <?> "tabular parser") =$= awaitForever (either (error . show) (yield . snd))

verboseHits :: (Monad m, MonadIO m, MonadThrow m) => Conduit ByteString m HitStream
verboseHits = decodeUtf8 =$= conduitParserEither (parseVerbose <?> "verbose parser") =$= awaitForever (either (error . show) (yield . snd))

parseTabular :: AT.Parser HitStream
parseTabular = return undefined

parseVerbose :: AT.Parser HitStream
parseVerbose = return undefined

-- | Stream of hits. The search or scan header indicate the source. Multi

data HitStream
    = SearchHeader
    { _version            :: Text
    , _queryCmFile        :: Text
    , _queryLength        :: Int
    , _targetSequenceFile :: Text
    , _workerThreads      :: Int
    }
    | SearchEntries
    {
    }
    | ScanHeader
    { _querySequenceFile  :: Text
    , _targetCmFile       :: Text
    , _workerThreads      :: Int
    }
    | ScanEntries
    { _queryHeader        :: Text
    , _queryDescription   :: Text
    }
    | Entry
    { _entry :: Hit
    }
    | Statistics
    { _unsortedStatistics :: [Text]
    }

makeLenses ''HitStream
makePrisms ''HitStream

