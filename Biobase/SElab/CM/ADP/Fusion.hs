
-- | (Temporary) location for fusion-related components.

module Biobase.SElab.CM.ADP.Fusion where

import           Control.DeepSeq
import           Control.Lens ( (^.), makeLenses, makePrisms )
import           Data.Aeson
import           Data.Binary
import           Data.Hashable (Hashable(..))
import           Data.Serialize (Serialize)
import           Data.Strict.Tuple hiding (fst,snd)
import           Data.Vector.Fusion.Stream.Monadic hiding (length,flatten)
import           Data.Vector.Generic (Vector, length, unsafeIndex)
import           Debug.Trace
import           GHC.Generics (Generic)
import           Prelude hiding (map,length,filter)

import           ADP.Fusion
import           ADP.Fusion.SynVar.Indices
import           Data.PrimitiveArray hiding (map, unsafeIndex)
import qualified Data.PrimitiveArray as PA

import           Biobase.SElab.CM.Types hiding (S)
import           Biobase.SElab.Bitscore



-- * Indexing into (fast) CMs.

-- | A 'StateIx' needs to carry around the tree topology. We do this by
-- carrying around the actual child targets from the @sTransitions@
-- structure.
--
-- NOTE we @hash@ only on the current index, not the tree topology!
--
-- TODO currently, any @INS@ state will be visited just once.

data StateIx t where
  StateIx ::
    { _siChilden  :: !(Unboxed (Z:.PInt I StateIndex:.Int) (PInt I StateIndex, Bitscore))
    , _siType     :: !(Unboxed (PInt I StateIndex)         StateType)
    , _siIx       :: !(PInt I StateIndex)
    , _siChild    :: !Int   -- ^ @-1@ or set to the child of @_siChildren@ we look at
    } -> StateIx t
  deriving (Show,Read,Generic)

makeLenses ''StateIx
makePrisms ''StateIx

mkStateIx :: CM -> StateIx I
mkStateIx cm = StateIx (cm^.states.sTransitions) (cm^.states.sStateType) 0 (-1)

instance Eq (StateIx t) where
  (StateIx _ _ x c) == (StateIx _ _ y d) = x == y && c == d
  {-# Inline (==) #-}

instance Ord (StateIx t) where
  (StateIx _ _ x c) <= (StateIx _ _ y d) = (x,c) <= (y,d)
  {-# Inline (<=) #-}

instance Hashable (StateIx t) where
  hashWithSalt s (StateIx _ _ p q) = hashWithSalt s (p,q)
  {-# Inline hashWithSalt #-}

instance Index (StateIx t) where
  linearIndex _ h (StateIx _ _ i _) = getPInt i
  {-# Inline linearIndex #-}
  smallestLinearIndex _ = error "still needed?"
  {-# Inline smallestLinearIndex #-}
  largestLinearIndex (StateIx _ _ h _) = getPInt h
  {-# Inline largestLinearIndex #-}
  size _ (StateIx _ _ h _) = getPInt h + 1
  {-# Inline size #-}
  inBounds _ (StateIx _ _ h _) (StateIx _ _ i _) = 0 <= i && i <= h
  {-# Inline inBounds #-}

-- TODO Need to write in accordance to @cs@. For now, assume correct ordering
--
-- (p.59 Infernal 1.1.1 manual) it is guaranteed that all children have
-- indices >= then the current index with (==) only in case of insert
-- self-transitions.
--
-- @streamUp@ actually needs to go from higher to lower states...

instance IndexStream z => IndexStream (z:.StateIx I) where
  streamUp (ls:.StateIx cs ty l _) (hs:.StateIx _ _ h _)
    = flatten mk step $ streamUp ls hs
    where mk s = return (s,h)
          step (s,i)
            | i < l     = return $ Done
            | otherwise = return $ Yield (s:.StateIx cs ty i (-1)) (s,i-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamUp #-}
  streamDown (ls:.StateIx cs ty l _) (hs:.StateIx _ _ h _)
    = flatten mk step $ streamDown ls hs
    where mk s = return (s,l)
          step (s,i)
            | i > h     = return $ Done
            | otherwise = return $ Yield (s:.StateIx cs ty i (-1)) (s,i+1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline streamDown #-}

instance IndexStream (StateIx I)

instance RuleContext (StateIx I) where
  type Context (StateIx I) = InsideContext ()
  initialContext _ = IStatic ()
  {-# Inline initialContext #-}

type instance TblConstraint (StateIx t) = TableConstraint

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
--
-- TODO use a function for membership, not a @StateType@ constant

data CMstate where
  CMstate :: (StateType -> Bool) -> States -> CMstate

type instance TermArg CMstate = States :. PInt () StateIndex

instance Build CMstate

instance
  ( Element ls i
  ) => Element (ls :!: CMstate) i where
  data Elm    (ls :!: CMstate) i = ElmCMstate !States !(PInt () StateIndex) !i !i !(Elm ls i)
  type Arg    (ls :!: CMstate)   = Arg ls :. (States :. PInt () StateIndex)
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
  ( TmkCtx1 m ls CMstate (StateIx t)
  ) => MkStream m (ls :!: CMstate) (StateIx t) where
  mkStream (ls :!: CMstate f xs) sv us is
    = map (\(ss,(eex:.eek),ii,oo) -> ElmCMstate eex eek ii oo ss)
    . addTermStream1 (CMstate f xs) sv us is
    $ mkStream ls (termStaticVar (CMstate f xs) sv is) us (termStreamIndex (CMstate f xs) sv is)
  {-# Inline mkStream #-}

-- |

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts CMstate) a (is:.StateIx I) where
  termStream (ts:|CMstate admit xs) (cs:._) (us:.u) (is:.ix@(StateIx _ styA i _)) -- same code for static+variable
    = map (\(TState s a b ii oo ee) ->
              TState s a b (ii:.ix) (oo:.ix) (ee:.(xs:.(PInt $ getPInt i))) )
    . termStream ts cs us is
    . staticCheck (admit $ styA ! i)  -- only allow going on if the type is admissable here
  {-# Inline termStream #-}

instance TermStaticVar CMstate (StateIx t) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}

-- * Emission of characters

-- | The 'EmitChar' terminal symbol does *not* match or parse characters
-- but rather emits each character one after another. The elements to be
-- emitted are given via the argument to the constructor.
--
-- TODO seems useful enough to put into ADPfusion itself at some point

data EmitChar c where
  EmitChar :: (Vector v c) => (v c) -> EmitChar c

type instance TermArg (EmitChar c) = c

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
  ( TmkCtx1 m ls (EmitChar c) (StateIx t)
  ) => MkStream m (ls :!: EmitChar c) (StateIx t) where
  mkStream (ls :!: EmitChar xs) sv us is
    = map (\(ss,ee,ii,oo) -> ElmEmitChar ee ii oo ss)
    . addTermStream1 (EmitChar xs) sv us is
    $ mkStream ls (termStaticVar (EmitChar xs) sv is) us (termStreamIndex (EmitChar xs) sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts (EmitChar c)) a (is:.StateIx I) where
  termStream (ts:|EmitChar xs) (cs:._) (us:.u) (is:.ix@(StateIx _ _ _ _))
    = flatten mk step . termStream ts cs us is
    where mk s = return (s :. length xs - 1)
          step (tstate@(TState s a b ii oo ee) :. k)
            | k < 0     = return $ Done
            | otherwise = return $ Yield (TState s a b (ii:.ix) (oo:.ix) (ee:.(unsafeIndex xs k)))
                                         (tstate :. k-1)
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

instance TermStaticVar (EmitChar c) (StateIx t) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}



-- * Epsilon state

-- |
--
-- TODO what about local ends? Probably means allowing @E@-like behaviour
-- with non-@E@ states. Infernal uses a special state @EL@ of which there
-- is one per model.

instance
  ( TmkCtx1 m ls Epsilon (StateIx t)
  ) => MkStream m (ls :!: Epsilon) (StateIx t) where
  mkStream (ls :!: Epsilon) sv us is
    = map (\(ss,ee,ii,oo) -> ElmEpsilon ii oo ss)
    . addTermStream1 Epsilon sv us is
    $ mkStream ls (termStaticVar Epsilon sv is) us (termStreamIndex Epsilon sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts Epsilon) a (is:.StateIx I) where
  termStream (ts :| Epsilon) (cs:._) (us:._) (is:.ix@(StateIx styC styA i _))
    = map (\(TState s a b ii oo ee) ->
              TState s a b (ii:.ix) (oo:.ix) (ee:.()) )
    . termStream ts cs us is
    . staticCheck (sty == E || sty == EL)
    where !sty = styA ! i
  {-# Inline termStream #-}



-- * Invisible starting symbol

instance
  ( Monad m
  ) => MkStream m S (StateIx I) where
  mkStream S (IStatic ()) (StateIx _ _ h _) (StateIx cs ty i _)
    = staticCheck (i>=0 && i<=h) . singleton $ ElmS (StateIx cs ty i (-1)) (StateIx cs ty (-1) (-1))
  mkStream S (IVariable ()) (StateIx _ _ h _) (StateIx cs ty i _)
    = filter (const $ 0<=i && i<=h) . singleton $ ElmS (StateIx cs ty i (-1)) (StateIx cs ty (-1) (-1))
  {-# Inline mkStream #-}



-- * Syntactic variables

{-

-- * Capturing transition scores
-- 
-- I'd rather return the transition scores together with the synvar, but
-- alas right now we can't.

-- | TODO maybe have a more interesting return? Maybe where we transitioned
-- from?
--
-- TODO not needed anymore?

data Transition = Transition
  deriving (Eq,Ord,Show)

instance
  ( Element ls i
  ) => Element (ls :!: Transition) i where
  data Elm (ls :!: Transition)    i = ElmTransition !Bitscore !i !i !(Elm ls i)
  type Arg (ls :!: Transition)      = Arg ls :. Bitscore
  type RecElm (ls :!: Transition) i = Elm ls i
  getArg (ElmTransition b _ _ ls) = getArg ls :. b
  getIdx (ElmTransition _ i _ _ ) = i
  getOmx (ElmTransition _ _ o _ ) = o
  getElm (ElmTransition _ _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}
  {-# Inline getElm #-}

instance
  ( Monad m
  , Element ls StateIx
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: Transition) StateIx where
  mkStream (ls :!: Transition) ctxt hh kk@(StateIx styC styA k _)
    = map (\s -> let k = getIdx s ^. siIx
                     c = getIdx s ^. siChild
                 in  ElmTransition (if c>=0 then (Prelude.snd $ styC ! (Z:.k:.c)) else 0) kk kk s)
    $ mkStream ls ctxt hh kk
  {-# Inline mkStream #-}



-- * syntactic variable

instance
  ( Monad m
  , PrimArrayOps arr StateIx x
  , MkStream m ls StateIx
  ) => MkStream m (ls :!: ITbl m arr StateIx x) StateIx where
  mkStream (ls :!: ITbl _ _ c t _) (IStatic ()) hh kk@(StateIx styC styA k _)
    = flatten mk step $ mkStream ls (IVariable ()) hh kk
    where mk s | sty == B  = return $ Just $ Left  s
               | otherwise = return $ Just $ Right (s,0)
          step Nothing  = return $ Done
          -- we have a branching state and extract the score for the
          -- *right* child
          step (Just (Left s)     ) = let ll = StateIx styC styA (fst $ styC ! (Z:.k:.1)) 1 -- right child
                                      in  return $ Yield (ElmITbl (t!ll) kk kk s) Nothing
          -- linear grammar case. Iterate over all children
          step (Just (Right (s,i)))
            | i > hI || (fst $ styC ! (Z:.k:.i)) < 0 = return $ Done
            -- TODO we skip here, because we do not want self-loops.
            -- These will require a slightly more involved index structure
            | (fst $ styC ! (Z:.k:.i)) == k  = return $ Skip (Just (Right (s,i+1)))
            | otherwise                      = let ll = StateIx styC styA (fst $ styC !(Z:.k:.i)) i
                                               in  return $ Yield (ElmITbl (t!ll) (StateIx styC styA k i) kk s) (Just (Right (s,i+1)))
          sty  = styA ! k
          (_,Z:._:. (!hI)) = bounds styC
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  mkStream (ls :!: ITbl _ _ c t _) (IVariable ()) hh kk@(StateIx styC styA k _)
    = flatten mk step $ mkStream ls (IVariable ()) hh kk
    where mk s | sty == B  = return $ Just s
               | otherwise = return $ Nothing
          step Nothing  = return $ Done
          step (Just s) = let ll = StateIx styC styA (fst $ styC !(Z:.k:.0)) 0 -- left child
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




-- * Multi-dimensional extensions


-- ** Extensions for Transition

-- type instance TermArg (TermSymbol a Transition) = TermArg a :. Bitscore
-- 
-- instance
--   ( Monad m
--   , TerminalStream m a is
--   ) => TerminalStream m (TermSymbol a Transition) (is:.StateIx) where
--   terminalStream (a:|Transition) (sv:.ctxt) (is:.i@(StateIx styC styA k _))
--     = map (\(S6 s (zi:.c') (zo:._) is os e) ->
--         let c = c' ^. siChild -- TODO ok?
--         in  S6 s zi zo (is:.i) (os:.i) (e :. (if c>=0 then (Prelude.snd $ styC ! (Z:.k:.c)) else 0)))
--     . iPackTerminalStream a sv (is:.i)
--   {-# Inline terminalStream #-}

instance TermStaticVar Transition StateIx where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline termStaticVar   #-}
  {-# Inline termStreamIndex #-}



-}

