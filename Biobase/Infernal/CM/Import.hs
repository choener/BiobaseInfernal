
-- | Iteratee-based parsing of Infernal covariance models.

module Biobase.Infernal.CM.Import where

import Data.Iteratee as I
import Data.Iteratee.Char as I
import Data.Iteratee.IO as I
import Data.Iteratee.Iteratee as I
import Data.Iteratee.ListLike as I
import Data.ByteString as BS

import Biobase.Infernal.CM



-- * iteratee stuff

-- | 

eneeCM :: (Monad m) => Enumeratee ByteString [CM] m a
eneeCM = enumLinesBS ><> convStream f where
  f = do
    th <- tryHead
    return undefined


-- * convenience functions

-- | Read covariance models from file. This parser reads one or more CMs from
-- file.

fromFile :: FilePath -> [CM]
fromFile = undefined
