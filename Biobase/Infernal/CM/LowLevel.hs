{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}

-- | Low-level covariance models suitable for high-performance computation.

module Biobase.Infernal.CM.LowLevel where

import Data.PrimitiveArray
import Data.PrimitiveArray.Unboxed.Zero
import Data.Array.Repa.Index
import Data.Primitive.Types
import Data.Map as M
import Data.Lens.Common
import Prelude as P
import Control.Arrow

import Biobase.Primary

import Biobase.Infernal.CM
import Biobase.Infernal.Types



-- | High-performance CM data structures. We use 'PrimitiveArray's and
-- flattened lookups. The state id is equal to the index into the different
-- data structures.

data CMll = CMll
  { _stateTypes  :: Arr0 (Z:.StateID) STll
  , _emitsSingle :: Arr0 (Z:.StateID:.Nuc) BitScore
  , _emitsPair   :: Arr0 (Z:.StateID:.Nuc:.Nuc) BitScore
  }

-- | 'StateType' low-level

newtype STll = STll {unSTll :: Int}
  deriving (Eq,Ord,Show,Read,Prim)

-- |

fromStateType :: StateType -> STll
fromStateType = STll . fromEnum

-- | Transform CM to low-level CM.

fromCM :: CM -> CMll
fromCM cm = CMll
  -- create low-level state type array from map. Needs to transfrom key of
  -- StateID into (Z:.StateID) and extract + transform the state type into
  -- low-leve STll.
  { _stateTypes  = fromAssocs (Z:.l) (Z:.h) (STll $ -1) . P.map ( (Z:.) *** (fromStateType . _stateType)) . M.toList $ cm ^. states
  , _emitsSingle = fromAssocs (Z:.l:.nN) (Z:.h:.nU) badScore $ [ ((Z:.sid:.nuc), bitscore)
                                                               | (sid,s) <- M.toList $ cm ^. states, let EmitsSingle xs = s ^. emits
                                                               , (c,bitscore) <- xs, let nuc = mkNuc c
                                                               ]
  , _emitsPair = fromAssocs (Z:.l:.nN:.nN) (Z:.h:.nN:.nN) badScore $ [ ((Z:.sid:.nuc1:.nuc2), bitscore)
                                                                     | (sid,s) <- M.toList $ cm ^. states, let EmitsPair xs = s ^. emits
                                                                     , ((c1,c2),bitscore) <- xs, let nuc1 = mkNuc c1, let nuc2 = mkNuc c2
                                                                     ]
  } where
      l = minimum . M.keys $ cm ^. states
      h = maximum . M.keys $ cm ^. states

badScore = BitScore $ -1000000
{-# INLINE badScore #-}
