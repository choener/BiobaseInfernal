{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- | Alignment as returned by @cmsearch@.

module Biobase.SElab.Hit.Alignment where

import           Control.Applicative
import           Control.Lens
import           Data.Text (Text)
import           Data.Vector.Generic.Lens
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics
import qualified Data.Text as T
import qualified Data.Vector.Generic
import qualified Data.Vector.Generic.Mutable
import qualified Data.Vector.Unboxed as VU



data Column = Column
            { _notCanBP  ::                !Bool
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

