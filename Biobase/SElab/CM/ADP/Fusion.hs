
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
import           Biobase.Types.NumericalExtremes

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
    { _siChilden  :: !(Unboxed (Z:.PInt () StateIndex:.Int) (PInt () StateIndex, Bitscore))
    , _siType     :: !(Unboxed (PInt () StateIndex)         StateType)
    , _siIx       :: !(PInt () StateIndex)
    , _siChild    :: !Int   -- ^ @-1@ or set to the child of @_siChildren@ we look at
    } -> StateIx t
  deriving (Show,Read,Generic)

makeLenses ''StateIx
makePrisms ''StateIx

mkStateIx0 :: States -> StateIx I
mkStateIx0 s = StateIx (s^.sTransitions) (s^.sStateType) 0 (-1)

mkStateIxH :: States -> StateIx I
mkStateIxH s =
  let (_,Z:.h:._) = bounds $ s^.sTransitions
  in  StateIx (s^.sTransitions) (s^.sStateType) h (-1)

-- | The running index for a 'StateIx' is the index we currently point to
-- as @PInt () StateIndex@ and the child number as @Int@.

data instance RunningIndex (StateIx I) = RiSixI !(PInt () StateIndex) !Int

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

--type instance TblConstraint (StateIx t) = TableConstraint

-- * 

-- | Capture the states of a covariance model and possibly restrict the
-- states for which rules succeed.
--
-- Assume that you want to write a CFG rule @X -> c X c@ which captures
-- that we have a pair-emitting state. We only want this rule to succeed
-- in pair cases and we want to expose the position-specific model
-- information. @CMstate@ allows this. We write
-- @
-- let sMP = CMstate ((==) MP) cm
-- in  [| X -> sMP c X c |]
-- @
-- somewhat abusing notation.
--
-- This terminal also primes the child generation system.
--
-- TODO If I were to proxify @StateType@ it would become possible to hand
-- over the correct pssm matrix in a type-safe way. Now, we hand over both
-- the cm, and the index. It is, however, much simpler in terms of
-- programming effort to do it the easy way.
--
-- TODO how about running time if I were to use a Proxy?
--
-- TODO use a function for membership, not a @StateType@ constant
--
-- TODO we might want to give TRANSITION scores with this thing? We would
-- have to populate the @children@ at this point as well?!

data CMstate where
  CMstate :: (StateType -> Bool) -> !States -> CMstate

-- | Shortcut for the transition-emission scores we need. Returns all
-- states, the @current@ index we are at, and the transition score to the
-- next lower state we want to visit at some point.

type CMte = States :. PInt () StateIndex :. Bitscore

type instance TermArg CMstate = CMte

instance Build CMstate

instance
  ( Element ls i
  ) => Element (ls :!: CMstate) i where
  data Elm    (ls :!: CMstate) i = ElmCMstate !States !(PInt () StateIndex) !Bitscore !(RunningIndex i) !(Elm ls i)
  type Arg    (ls :!: CMstate)   = Arg ls :. (States :. PInt () StateIndex :. Bitscore)
  type RecElm (ls :!: CMstate) i = Elm ls i
  getArg (ElmCMstate s k b _ ls) = getArg ls :. (s:.k:.b)
  getIdx (ElmCMstate _ _ _ i _ ) = i
  getElm (ElmCMstate _ _ _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getElm #-}

instance
  ( TmkCtx1 m ls CMstate (StateIx t)
  ) => MkStream m (ls :!: CMstate) (StateIx t) where
  mkStream (ls :!: CMstate f xs) sv us is
    = map (\(ss,(eex:.eek:.eeb),ii) -> ElmCMstate eex eek eeb ii ss)
    . addTermStream1 (CMstate f xs) sv us is
    $ mkStream ls (termStaticVar (CMstate f xs) sv is) us (termStreamIndex (CMstate f xs) sv is)
  {-# Inline mkStream #-}

-- | Will populate the index with the first child! Depending on the current
-- state type, we'll have to do different things.
--
-- If we have a branching state, we only set the first child, as the 2nd
-- synvar will grab the 2nd child.
--
-- Otherwise, we'll go through all children. This will also net us the
-- transition score.

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts CMstate) a (is:.StateIx I) where
  termStream (ts:|CMstate admit xs) (cs:._) (us:.u) (is:.ix@(StateIx styC styA i _)) -- same code for static+variable
    = flatten mk step
--    . map (\x -> traceShow (getPInt i) x)
    . termStream ts cs us is
    where mk s = if (admit stya) then return $ Just (s :. 0) else return Nothing
          step Nothing = return $ Done
          step (Just (tstate@(TState s a ii ee) :. c))
            -- if we @B@ranch, then the 2nd child is consumed by the static
            -- synvar!
            | stya == B = return $ Yield (TState s a (ii:.:j) (ee:.e)) Nothing
            -- this state has no children! It is an @E@ or @EL@ state. We
            -- don't check on styc, because end states have none. We also
            -- try very, very hard to show that there is not next child
            -- here.
            | stya == E || stya == EL  = let e0 = xs:.i:.0
                                         in  return $ Yield (TState s a (ii:.:RiSixI (-1) (-1)) (ee:.e0)) Nothing
            -- no more valid children left. Assumes that all valid children
            -- are stored consecutively.
            | c>5 || styc < 0 = return $ Done
            -- this child was given a very bad transition score, we skip.
            -- TODO use a constant like 'verySmall', not this hardcoded
            -- thing.
            | trns <= -10000 = return $ Skip $ Just (tstate :. c+1)
            -- normal state with many children
            | otherwise = return $ Yield (TState s a (ii:.:j) (ee:.e)) (Just (tstate :. c+1))
            where (styc,trns) = styC ! (Z:.i:.c)
                  RiSixI chd _ = getIndex a (Proxy :: PRI is (StateIx I))
                  j = RiSixI styc c
                  e = xs:.i:.trns -- states structure, our index (important when requesting emission scores), transition score to next child
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
          !stya = styA ! i
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
  EmitChar :: (Vector v c) => !(v c) -> EmitChar c

type instance TermArg (EmitChar c) = c

instance Build (EmitChar c)

instance
  ( Element ls i
  ) => Element (ls :!: EmitChar c) i where
  data Elm (ls :!: EmitChar c) i = ElmEmitChar !c !(RunningIndex i) !(Elm ls i)
  type Arg (ls :!: EmitChar c)   = Arg ls :. c
  type RecElm (ls :!: EmitChar c) i = Elm ls i
  getArg (ElmEmitChar c _ ls) = getArg ls :. c
  getIdx (ElmEmitChar _ i _ ) = i
  getElm (ElmEmitChar _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getElm #-}

instance
  ( TmkCtx1 m ls (EmitChar c) (StateIx t)
  ) => MkStream m (ls :!: EmitChar c) (StateIx t) where
  mkStream (ls :!: EmitChar xs) sv us is
    = map (\(ss,ee,ii) -> ElmEmitChar ee ii ss)
    . addTermStream1 (EmitChar xs) sv us is
    $ mkStream ls (termStaticVar (EmitChar xs) sv is) us (termStreamIndex (EmitChar xs) sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts (EmitChar c)) a (is:.StateIx I) where
  termStream (ts:|EmitChar xs) (cs:._) (us:.u) (is:.ix@(StateIx _ _ _ _))
    = flatten mk step . termStream ts cs us is
    where mk s = return (s :. length xs - 1)
          step (tstate@(TState s a ii ee) :. k)
            | k < 0     = return $ Done
            | otherwise = return $ Yield (TState s a (ii:.:j) (ee:.(unsafeIndex xs k)))
                                         (tstate :. k-1)
            where j = getIndex a (Proxy :: PRI is (StateIx I))
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
    = map (\(ss,ee,ii) -> ElmEpsilon ii ss)
    . addTermStream1 Epsilon sv us is
    $ mkStream ls (termStaticVar Epsilon sv is) us (termStreamIndex Epsilon sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts Epsilon) a (is:.StateIx I) where
  termStream (ts :| Epsilon) (cs:._) (us:._) (is:.ix@(StateIx styC styA i _))
    = map (\(TState s a ii ee) ->
              let j = getIndex a (Proxy :: PRI is (StateIx I))
              in  TState s a (ii:.:j) (ee:.()) )
    . termStream ts cs us is
    . filter (\_ -> sty==E || sty==EL)
--    . staticCheck (sty == E || sty == EL)
    where !sty = styA ! i
  {-# Inline termStream #-}

instance TermStaticVar Epsilon (StateIx t) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}



-- * Deletion Terminal

instance
  ( TmkCtx1 m ls Deletion (StateIx t)
  ) => MkStream m (ls :!: Deletion) (StateIx t) where
  mkStream (ls :!: Deletion) sv us is
    = map (\(ss,ee,ii) -> ElmDeletion ii ss)
    . addTermStream1 Deletion sv us is
    $ mkStream ls (termStaticVar Deletion sv is) us (termStreamIndex Deletion sv is)
  {-# Inline mkStream #-}

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts Deletion) a (is:.StateIx I) where
  termStream (ts :| Deletion) (cs:._) (us:._) (is:.ix@(StateIx styC styA i _))
    = map (\(TState s a ii ee) ->
              let j = getIndex a (Proxy :: PRI is (StateIx I))
              in  TState s a (ii:.:j) (ee:.()) )
    . termStream ts cs us is
  {-# Inline termStream #-}

instance TermStaticVar Deletion (StateIx t) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}



-- * Bind a synvar terminally.

-- | This terminal makes it possible to bind a synvar terminally. Useful
-- for multidim grammars to exclude complete substates. Will return the
-- index being ignored.
--
-- TODO Move into ADPfusion?

data Terminally x = Terminally

terminally :: x -> Terminally x
terminally _ = Terminally
{-# Inline terminally #-}

type instance TermArg (Terminally x) = x

instance Build (Terminally x)

instance
  ( Element ls i
  ) => Element (ls :!: Terminally x) i where
  data Elm (ls :!: Terminally x) i = ElmTerminally !x !(RunningIndex i) !(Elm ls i)
  type Arg (ls :!: Terminally x)   = Arg ls :. x
  type RecElm (ls :!: Terminally c) i = Elm ls i
  getArg (ElmTerminally c _ ls) = getArg ls :. c
  getIdx (ElmTerminally _ i _ ) = i
  getElm (ElmTerminally _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getElm #-}

instance
  ( TmkCtx1 m ls (Terminally x) (StateIx t)
  ) => MkStream m (ls :!: Terminally x) (StateIx t) where
  mkStream (ls :!: t) sv us is
    = map (\(ss,ee,ii) -> ElmTerminally ee ii ss)
    . addTermStream1 Terminally sv us is
    $ mkStream ls (termStaticVar t sv is) us (termStreamIndex t sv is)
  {-# Inline mkStream #-}

-- 
--
-- TODO Requires flatten so that we can check that no weird child is
-- requested.

instance
  ( TstCtx1 m ts a is (StateIx I)
  ) => TermStream m (TermSymbol ts (Terminally (StateIx I))) a (is:.StateIx I) where
  termStream (ts:|Terminally) (cs:.IStatic ()) (us:._) (is:.ix@(StateIx styC styA i _))
    = flatten mk step . termStream ts cs us is
    where mk (tstate@(TState s a ii ee)) =
            let RiSixI chd c = getIndex a (Proxy :: PRI is (StateIx I))
            in  return (tstate :. chd :. c)
          step (tstate@(TState s a ii ee) :. chd :. c)
            | chd < 0 = return $ Done
            | otherwise = let kt = StateIx styC styA chd (-1)
                          in  return $ Yield (TState s a (ii:.:RiSixI chd (-1)) (ee:.kt)) (tstate:.(-1):.(-1))
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  termStream (ts:|Terminally) (cs:.IVariable ()) (us:._) (is:.ix@(StateIx styC styA i _))
    = flatten mk step . termStream ts cs us is
    where mk (tstate@(TState s a ii ee)) =
            let RiSixI chd c = getIndex a (Proxy :: PRI is (StateIx I))
            in  return (tstate :. chd :. c)
          step (tstate@(TState s a ii ee) :. chd :. c)
            | chd < 0 = return $ Done
            | otherwise = let (!chd',!c') = if c < 0 || c > 5
                                            then (-1,-1)
                                            else (fst $ styC!(Z:.i:.c), c+1)
                              kt = StateIx styC styA chd c
                          in  return $ Yield (TState s a (ii:.:RiSixI chd c) (ee:.kt)) (tstate:.chd':.c')
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline termStream #-}

-- | We need exactly the same behaviour as with synvars!

instance
  (
  ) => TermStaticVar (Terminally x) (StateIx I) where
  termStaticVar _ _ _ = IVariable ()
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}



-- * Invisible starting symbol

-- | Create the initial StateIx. The "next smaller" synvar we point to is
-- the current index. This means that @X -> X@ does actually loop (which is
-- good). Next child is set to @(-1)@, stating that we have not even
-- started touching the children state.
--
-- TODO not using 'staticCheck' for now. Test later!

instance
  ( Monad m
  ) => MkStream m S (StateIx I) where
  mkStream S (IStatic ()) (StateIx _ _ u _) (StateIx cs ty i _)
    = filter (\_ -> 0<=i && i<=u) -- staticCheck (i>=0 && i<=h)
    . singleton $ ElmS $ RiSixI i (-1)
  mkStream S (IVariable ()) (StateIx _ _ u _) (StateIx cs ty i _)
    = filter (\_ -> 0<=i && i<=u)
    . singleton $ ElmS $ RiSixI i (-1)
  {-# Inline mkStream #-}

instance
  ( Monad m
  , MkStream m S is
  ) => MkStream m S (is:.StateIx I) where
  mkStream S (vs:.IStatic ()) (us:.StateIx _ _ u _) (is:.StateIx c t i _)
    = map (\(ElmS zi) -> ElmS $ zi :.: RiSixI i (-1))
    . filter (\_ -> 0<=i && i<=u) -- staticCheck (i>=0 && i<=u)
    $ mkStream S vs us is
  mkStream S (vs:.IVariable ()) (us:.StateIx _ _ u _) (is:.StateIx c t i _)
    = map (\(ElmS zi) -> ElmS $ zi :.: RiSixI i (-1))
    . filter (\_ -> 0<=i && i<=u) -- staticCheck (i>=0 && i<=u)
    $ mkStream S vs us is
  {-# Inline mkStream #-}

-- * Syntactic variables
--
-- Table:   Inside
-- Grammar: Inside
--
-- Each grammar should have 0, 1, or 2 syntactic variables per rule, not
-- more. We have 1 synvars for @B@ranching states, @1@ for most others.
-- Those with 0 synvars are not interesting here.
--
-- Depending on the state (branch or not) have to handle two different
-- cases.

instance
  ( IndexHdr a us (StateIx I) cs c is (StateIx I)
  ) => AddIndexDense a (us:.StateIx I) (cs:.c) (is:.StateIx I) where
  addIndexDenseGo (cs:.c) (vs:.IStatic ()) (us:._) (is:.ix@(StateIx styC styA i _))
    = flatten mk step . addIndexDenseGo cs vs us is
    where mk (svs@(SvS s a tt ii)) =
            let RiSixI chd c = getIndex a (Proxy :: PRI is (StateIx I))
            in  return $ (svs :. chd :. c)
          step (svs@(SvS s a tt ii) :. chd :. c)
            -- we have no way to go
            | chd < 0   = return $ Done
            | otherwise = let kt = StateIx styC styA chd (-1)
                          in  {- traceShow ('C',getPInt chd) . -} return $ Yield (SvS s a (tt:.kt) (ii:.: RiSixI chd (-1))) (svs :. (-1) :. (-1))
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  addIndexDenseGo (cs:.c) (vs:.IVariable ()) (us:._) (is:.ix@(StateIx styC styA i _))
    = flatten mk step . addIndexDenseGo cs vs us is
    where mk (svs@(SvS s a tt ii)) =
            let RiSixI chd c = getIndex a (Proxy :: PRI is (StateIx I))
            in  return $ (svs :. chd :. c)
          step (svs@(SvS s a tt ii) :. chd :. c)
            | chd < 0 = return $ Done
            | otherwise = let (!chd',!c') = if c < 0 || c > 5
                                            then (-1,-1)
                                            else (fst $ styC!(Z:.i:.c), c+1)
                              kt = StateIx styC styA chd c
                          in  return $ Yield (SvS s a (tt:.kt) (ii:.: RiSixI chd c)) (svs :. chd' :. c')
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
  {-# Inline addIndexDenseGo #-}

instance TableStaticVar (StateIx I) c (StateIx I) where
  tableStaticVar   _ _ _ _ = IVariable ()
  tableStreamIndex _ _ _ s = s
  {-# Inline [0] tableStaticVar   #-}
  {-# Inline [0] tableStreamIndex #-}

