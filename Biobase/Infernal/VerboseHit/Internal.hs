
-- | Shared, internal stuff.

module Biobase.Infernal.VerboseHit.Internal where

import qualified Data.ByteString.Char8 as BS



-- | State for import and export functions

data AliGo = AliGo
  { aliCM :: BS.ByteString
  , aliScaffold :: BS.ByteString
  , aliStrand :: Char
  } deriving (Show)

