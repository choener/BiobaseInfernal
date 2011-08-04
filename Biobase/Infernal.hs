
-- | Re-export the most import parts.

module Biobase.Infernal
  ( TabularHit(..)
  , thFromFile
  , eneeTabularHit
  , VerboseHit(..)
  , vhFromFile
  , eneeVerboseHit
  , vhEneeByteString
  , vhEneeByteStrings
  , Species(..)
  , tFromFile
  , Clan(..)
  , cFromFile
  ) where

import Data.ByteString as BS
import Data.Iteratee as I

import Biobase.Infernal.TabularHit
import Biobase.Infernal.TabularHit.Import as TH
import Biobase.Infernal.VerboseHit
import Biobase.Infernal.VerboseHit.Import as VH
import Biobase.Infernal.VerboseHit.Export as VH
import Biobase.Infernal.Taxonomy
import Biobase.Infernal.Taxonomy.Import as T
import Biobase.Infernal.Clan
import Biobase.Infernal.Clan.Import as C

thFromFile = TH.fromFile

vhFromFile = VH.fromFile
vhEneeByteString :: Monad m => Enumeratee [VerboseHit] ByteString m a
vhEneeByteString = VH.eneeByteString
vhEneeByteStrings :: Monad m => Enumeratee [VerboseHit] [ByteString] m a
vhEneeByteStrings = VH.eneeByteStrings

tFromFile = T.fromFile

cFromFile = C.fromFile
