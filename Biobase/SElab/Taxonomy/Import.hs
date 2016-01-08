
-- | Import an Rfam @taxonomy.txt@ file. Provides a simple "fromFile"
-- function that produces both maps in one pass. @fromFile@ will check the
-- file suffix and on @.gz@ suffixes decompress the input on-the-fly.

module Biobase.SElab.Taxonomy.Import where

import           Codec.Compression.GZip (decompress)
import           Control.Applicative ((<|>))
import           Control.Arrow (second)
import           Control.Monad
import           Data.Attoparsec.Text.Lazy as AT
import           Data.Char (isDigit)
import           Data.HashMap.Strict (HashMap)
import           Data.List (foldl')
import           Data.Stringable (fromText)
import           Data.Text.Lazy.Encoding (decodeUtf8)
import           Data.Text.Lazy.IO as TL
import           Data.Text (Text)
import           Data.Vector (fromList)
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import           System.FilePath (takeExtension)

import Biobase.Types.Accession (Accession(..),Species)
import Biobase.Types.Names
import Biobase.Types.Taxonomy



-- | Parse a single Taxon line.
--
-- TODO there are unknown words at the end of each line. make those known

parseTaxon :: Parser Taxon
parseTaxon = do
  accession <- Accession <$> takeWhile1 isDigit <?> "accession"
  skipSpace <?> "1st space"
  species <- speciesName <$> takeWhile1 (/='\t') <?> "species"
  skipSpace <?> "2nd space"
  classification <- (fromList . map (,Unknown) . map fromText) <$> takeWhile1 (\z -> z/=';' && z/='.') `sepBy` "; " <?> "classification"
  unknowns <- manyTill anyChar (endOfInput <|> endOfLine)
  return $ Taxon {..}

-- | Taxonomy according to @Infernal@ stored in two hashmaps. The first
-- from @Accession@ to @Taxon@, the second from species name to @Taxon@.

type Taxonomy = ( HashMap (Accession Species) Taxon   -- ^ find @Taxon@ via accession number
                , HashMap SpeciesName         Taxon   -- ^ find @Taxon@ via species name
                )

-- | Parses the taxonomy.txt file.

parseTaxonomy :: Parser Taxonomy
parseTaxonomy = foldl' go (HM.empty , HM.empty) <$> manyTill parseTaxon endOfInput
  where go (!a, !s) x = (HM.insert (accession x) x a, HM.insert (species x) x s)

-- | Read @taxonomy.txt.gz@ / @taxonomy.txt@ file into structure.

fromFile :: FilePath -> IO Taxonomy
fromFile f = case takeExtension f of
  ".gz" -> (go . decodeUtf8 . decompress) <$> BL.readFile f
  _     -> go <$> TL.readFile f
  where go txt = case AT.parse parseTaxonomy txt of
          Done ""   r       -> r
          Done ncon r       -> error $ "unconsumed input " ++ f ++ ": " ++ (TL.unpack $ TL.take 1000 ncon)
          Fail ncon ctx err -> error $ "error parsing " ++ f ++ ": " ++ show (ctx,err)

