
-- | (Temporary) location for fusion-related components.

module Biobase.SElab.CM.ADP.Fusion where

import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic
import Prelude hiding (map)
import GHC.Generics
import Data.Hashable (Hashable(..))
import Data.Aeson
import Data.Binary
import Data.Serialize
import Control.DeepSeq

import ADP.Fusion
import Data.PrimitiveArray hiding (map)

import Biobase.SElab.CM.Types



-- * Indexing into (fast) CMs.

-- | A 'StateIx' needs to carry around the tree topology. We do this by
-- carrying around the actual child targets from the @sTransitions@
-- structure.
--
-- NOTE we @hash@ only on the current index, not the tree topology!
--
-- TODO currently, any @INS@ state will be visited just once.

data StateIx where
  StateIx
    :: !(Unboxed (Z:.PInt StateIndex:.Int) (PInt StateIndex))
    -> !(PInt StateIndex)
    -> StateIx
  deriving (Show,Read,Generic)

instance Eq StateIx where
  (StateIx _ x) == (StateIx _ y) = x == y
  {-# Inline (==) #-}

instance Ord StateIx where
  (StateIx _ x) <= (StateIx _ y) = x <= y
  {-# Inline (<=) #-}

instance Hashable StateIx where
  hashWithSalt s (StateIx _ p) = hashWithSalt s p
  {-# Inline hashWithSalt #-}


-- * 

-- | Capture the states of a covariance model and possibly restrict the
-- states for which rules succeed.
--
-- Assume that you want to write a CFG rule @X -> c X c@ which captures
-- that we have a pair-emitting state. We only want this rule to succeed
-- in pair cases and we want to expose the position-specific model
-- information. @CMstate@ allows this. We write
-- @
-- let sMP = CMstate MP cm
-- in  [| X -> sMP c X c |]
-- @
-- somewhat abusing notation.
--
-- TODO If I were to proxify @StateType@ it would become possible to hand
-- over the correct pssm matrix in a type-safe way. Now, we hand over both
-- the cm, and the index. It is, however, much simpler in terms of
-- programming effort to do it the easy way.
--
-- TODO how about running time if I were to use a Proxy?

data CMstate where
  CMstate :: StateType -> States -> CMstate

instance Build CMstate

instance
  ( Element ls i
  ) => Element (ls :!: CMstate) i where
  data Elm    (ls :!: CMstate) i = ElmCMstate !States !(PInt StateIndex) !i !i !(Elm ls i)
  type Arg    (ls :!: CMstate)   = Arg ls :. (States :. PInt StateIndex)
  type RecElm (ls :!: CMstate) i = Elm ls i
  getArg (ElmCMstate s k _ _ ls) = getArg ls :. (s:.k)
  getIdx (ElmCMstate _ _ i _ _ ) = i
  getOmx (ElmCMstate _ _ _ o _ ) = o
  getElm (ElmCMstate _ _ _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}
  {-# Inline getElm #-}

instance
  ( Monad m
  , MkStream m ls (PInt StateIndex)
  ) => MkStream m (ls :!: CMstate) (PInt StateIndex) where
  mkStream (ls :!: CMstate s cm) ctxt hh kk@(PInt k)
    = staticCheck (s == (_sStateType cm ! (Z:.kk)))
    . map (\s -> ElmCMstate cm kk kk (PInt (-1)) s)
    $ mkStream ls ctxt hh kk
  {-# Inline mkStream #-}

instance RuleContext (PInt StateIndex) where
  type Context (PInt StateIndex) = InsideContext ()
  initialContext _ = IStatic ()
  {-# Inline initialContext #-}

