
-- | (Temporary) location for fusion-related components.

module Biobase.SElab.CM.ADP.Fusion where

import Control.DeepSeq
import Data.Aeson
import Data.Binary
import Data.Hashable (Hashable(..))
import Data.Serialize (Serialize)
import Data.Strict.Tuple
import Data.Vector.Fusion.Stream.Monadic hiding (length)
import Data.Vector.Fusion.Stream.Size
import Data.Vector.Generic (Vector, length, unsafeIndex)
import GHC.Generics (Generic)
import Prelude hiding (map,length,filter)

import ADP.Fusion
import Data.PrimitiveArray hiding (map, unsafeIndex)

import Biobase.SElab.CM.Types hiding (S)



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
    -> !(Unboxed (PInt StateIndex)         StateType)
    -> !(PInt StateIndex)
    -> StateIx
  deriving (Show,Read,Generic)

instance Eq StateIx where
  (StateIx _ _ x) == (StateIx _ _ y) = x == y
  {-# Inline (==) #-}

instance Ord StateIx where
  (StateIx _ _ x) <= (StateIx _ _ y) = x <= y
  {-# Inline (<=) #-}

instance Hashable StateIx where
  hashWithSalt s (StateIx _ _ p) = hashWithSalt s p
  {-# Inline hashWithSalt #-}

instance Index StateIx where
  linearIndex _ h (StateIx _ _ i) = getPInt i
  {-# Inline linearIndex #-}
  smallestLinearIndex _ = error "still needed?"
  {-# Inline smallestLinearIndex #-}
  largestLinearIndex (StateIx _ _ h) = getPInt h
  {-# Inline largestLinearIndex #-}
  size _ (StateIx _ _ h) = getPInt h + 1
  {-# Inline size #-}
  inBounds _ (StateIx _ _ h) (StateIx _ _ i) = 0 <= i && i <= h
  {-# Inline inBounds #-}

-- TODO Need to write in accordance to @cs@. For now, assume correct ordering

instance IndexStream z => IndexStream (z:.StateIx) where
  streamUp (ls:.StateIx cs ty l) (hs:.StateIx _ _ h)
    = flatten mk step Unknown $ streamUp ls hs
    where mk s = return (s,l)
          step (s,i)
            | i > h     = return $ Done
            | otherwise = return $ Yield (s:.StateIx cs ty i) (s,i+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.StateIx cs ty l) (hs:.StateIx _ _ h)
    = flatten mk step Unknown $ streamDown ls hs
    where mk s = return (s,h)
          step (s,i)
            | i < l     = return $ Done
            | otherwise = return $ Yield (s:.StateIx cs ty i) (s,i-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream StateIx

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
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: CMstate) StateIx where
  mkStream (ls :!: CMstate s cm) ctxt hh kk@(StateIx _ styA k)
    = staticCheck (s == styA ! k)
    . map (\s -> ElmCMstate cm k kk kk s)
    $ mkStream ls ctxt hh kk
  {-# Inline mkStream #-}

-- * Emission of characters

-- | The 'EmitChar' terminal symbol does *not* match or parse characters
-- but rather emits each character one after another. The elements to be
-- emitted are given via the argument to the constructor.

data EmitChar c where
  EmitChar :: (Vector v c) => (v c) -> EmitChar c

instance
  ( Element ls i
  ) => Element (ls :!: EmitChar c) i where
  data Elm (ls :!: EmitChar c) i = ElmEmitChar !c !i !i !(Elm ls i)
  type Arg (ls :!: EmitChar c)   = Arg ls :. c
  type RecElm (ls :!: EmitChar c) i = Elm ls i
  getArg (ElmEmitChar c _ _ ls) = getArg ls :. c
  getIdx (ElmEmitChar _ i _ _ ) = i
  getOmx (ElmEmitChar _ _ o _ ) = o
  getElm (ElmEmitChar _ _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}
  {-# Inline getElm #-}

instance
  ( Monad m
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: EmitChar c) StateIx where
  mkStream (ls :!: EmitChar vc) ctxt hh kk
    = flatten mk step Unknown $ mkStream ls ctxt hh kk
    where mk s = return (s :. length vc -1)
          step (s :. z)
            | z < 0     = return $ Done
            | otherwise = return $ Yield (ElmEmitChar (unsafeIndex vc z) kk kk s) (s :. z-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline mkStream #-}

-- * capturing end states
--
-- TODO what about local ends? Probably means allowing @E@-like behaviour
-- with non-@E@ states. Infernal uses a special state @EL@ of which there
-- is one per model.

instance
  ( Monad m
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: Epsilon) StateIx where
  mkStream (ls :!: Epsilon) (IStatic ()) hh kk@(StateIx styC styA k)
    = staticCheck (sty == E || sty == EL)
    . map (\s -> ElmEpsilon kk kk s)
    $ mkStream ls (IStatic ()) hh kk
    where !sty = styA ! k
  {-# Inline mkStream #-}

-- * syntactic variable

instance
  ( Monad m
  , PrimArrayOps arr StateIx x
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: ITbl m arr StateIx x) StateIx where
  mkStream (ls :!: ITbl _ _ c t _) (IStatic ()) hh kk@(StateIx styC styA k)
    = flatten mk step Unknown $ mkStream ls (IVariable ()) hh kk
    where mk s | sty == B  = return $ Just $ Left  s
               | otherwise = return $ Just $ Right (s,0)
          step Nothing  = return $ Done
          -- we have a branching state and extract the score for the
          -- *right* child
          step (Just (Left s)     ) = let ll = StateIx styC styA $ styC ! (Z:.k:.1) -- right child
                                      in  return $ Yield (ElmITbl (t!ll) kk kk s) Nothing
          -- linear grammar case. Iterate over all children
          step (Just (Right (s,i)))
            | i > 5 || styC ! (Z:.k:.i) < 0 = return $ Done
            | otherwise                     = let ll = StateIx styC styA $ styC !(Z:.k:.i)
                                              in  return $ Yield (ElmITbl (t!ll) kk kk s) (Just (Right (s,i+1)))
          sty  = styA ! k
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  mkStream (ls :!: ITbl _ _ c t _) (IVariable ()) hh kk@(StateIx styC styA k)
    = flatten mk step Unknown $ mkStream ls (IVariable ()) hh kk
    where mk s | sty == B  = return $ Just s
               | otherwise = return $ Nothing
          step Nothing  = return $ Done
          step (Just s) = let ll = StateIx styC styA $ styC !(Z:.k:.1)
                          in  return $ Yield (ElmITbl (t!ll) kk kk s) Nothing
          sty  = styA ! k
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline mkStream #-}

-- * rule context stuff

instance RuleContext StateIx where
  type Context StateIx = InsideContext ()
  initialContext _ = IStatic ()
  {-# Inline initialContext #-}

type instance TblConstraint StateIx = TableConstraint

instance
  ( Monad m
  ) => MkStream m S StateIx where
  mkStream S (IStatic ()) (StateIx _ _ h) (StateIx cs ty i)
    = staticCheck (i>=0 && i<=h) . singleton $ ElmS (StateIx cs ty i) (StateIx cs ty (-1))
  mkStream S (IVariable ()) (StateIx _ _ h) (StateIx cs ty i)
    = filter (const $ 0<=i && i<=h) . singleton $ ElmS (StateIx cs ty i) (StateIx cs ty (-1))
  {-# Inline mkStream #-}


