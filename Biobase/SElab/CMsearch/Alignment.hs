{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Alignment as returned by @cmsearch@.

module Biobase.SElab.CMsearch.Alignment where

import           Data.Text (Text)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU
import           Control.Applicative
import           Control.Lens
import           Data.Vector.Generic.Lens



data Column = Column
            { _notCanBP  :: {-# UNPACK #-} !Bool
            , _structure :: {-# UNPACK #-} !Char
            , _queryCons :: {-# UNPACK #-} !Char
            , _fit       :: {-# UNPACK #-} !Char
            , _target    :: {-# UNPACK #-} !Char
            , _posterior :: {-# UNPACK #-} !Char
            }
            deriving (Eq,Show,Generic)

makeLenses ''Column

derivingUnbox "Column"
  [t| Column -> (Bool,Char,Char,Char,Char,Char) |]
  [| (,,,,,) <$> _notCanBP <*> _structure <*> _queryCons <*> _fit <*> _target <*> _posterior |]
  [| \(n,s,q,a,t,p) -> Column n s q a t p |]

-- | An alignment of parts of a genome to an RNA family model.

data Alignment = Alignment
               { _alignment :: !(VU.Vector Column)
               }
               deriving (Eq,Show,Generic)

makeLenses ''Alignment

