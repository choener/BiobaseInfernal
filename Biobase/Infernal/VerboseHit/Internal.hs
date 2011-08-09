
-- | Shared, internal stuff.

module Biobase.Infernal.VerboseHit.Internal where

import Data.ByteString.Char8 as BS



-- | State for import and export functions

data AliGo = AliGo
  { aliCM :: ByteString
  , aliScaffold :: ByteString
  , aliStrand :: Char
  , aliAnnotation :: [ByteString]
  } deriving (Show)

