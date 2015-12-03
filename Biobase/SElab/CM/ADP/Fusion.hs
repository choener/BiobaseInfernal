
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
--
-- TODO we might want to give TRANSITION scores with this thing? We would
-- have to populate the @children@ at this point as well?!

data CMstate where
  CMstate :: (StateType -> Bool) -> States -> CMstate

-- | Shortcut for the transition-emission scores we need. Returns all
-- states, the @current@ index we are at, and the transition score to the
-- next lower state we want to visit at some point.

type CMte = States :. PInt () StateIndex :. Bitscore

type instance TermArg CMstate = CMte

instance Build CMstate

instance
  ( Element ls i
  ) => Element (ls :!: CMstate) i where
  data Elm    (ls :!: CMstate) i = ElmCMstate !States !(PInt () StateIndex) !Bitscore !i !i !(Elm ls i)
  type Arg    (ls :!: CMstate)   = Arg ls :. (States :. PInt () StateIndex :. Bitscore)
  type RecElm (ls :!: CMstate) i = Elm ls i
  getArg (ElmCMstate s k b _ _ ls) = getArg ls :. (s:.k:.b)
  getIdx (ElmCMstate _ _ _ i _ _ ) = i
  getOmx (ElmCMstate _ _ _ _ o _ ) = o
  getElm (ElmCMstate _ _ _ _ _ ls) = ls
  {-# Inline getArg #-}
  {-# Inline getIdx #-}
  {-# Inline getOmx #-}
  {-# Inline getElm #-}

instance
  ( TmkCtx1 m ls CMstate (StateIx t)
  ) => MkStream m (ls :!: CMstate) (StateIx t) where
  mkStream (ls :!: CMstate f xs) sv us is
    = map (\(ss,(eex:.eek:.eeb),ii,oo) -> ElmCMstate eex eek eeb ii oo ss)
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
    . termStream ts cs us is
--    . staticCheck (admit $ styA ! i)  -- only allow going on if the type is admissable here (TODO: better here or in step?)
    where mk s = if (admit stya) then return $ Just (s, 0) else return Nothing
          step Nothing = return $ Done
          step (Just (tstate@(TState s a b ii oo ee), c))
            -- if we @B@ranch, then the 2nd child is consumed by the static
            -- synvar!
            -- TODO i think, the @B@ and @E@ cases can be merged
            | stya == B = return $ Yield (TState s a b (ii:.k) (oo:.k) (ee:.e)) Nothing
            -- this state has no children! It is an @E@ or @EL@ state. We
            -- don't check on styc, because end states have none.
            | stya == E || stya == EL  = return $ Yield (TState s a b (ii:.k) (oo:.k) (ee:.e)) Nothing
            -- no more valid children left. Assumes that all valid children
            -- are stored consecutively.
            | c>5 || styc < 0 = return $ Done
            -- this child was given a very bad transition score, we skip.
            | trns <= Bitscore verySmall = return $ Skip $ Just (tstate,c+1)
            -- normal state with many children
            | otherwise = return $ Yield (TState s a b (ii:.k) (oo:.k) (ee:.e)) (Just (tstate, c+1))
            where (styc,trns) = styC ! (Z:.i:.c)
                  k = StateIx styC styA i c
                  e = xs:.i:.trns -- states structure, child index, transition score
          {-# Inline [0] mk   #-}
          {-# Inline [0] step #-}
          stya = styA ! i
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

instance Build (EmitChar c)

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
            | otherwise = return $ Yield (TState s a b (ii:.j) (oo:.j) (ee:.(unsafeIndex xs k)))
                                         (tstate :. k-1)
            where j = getIndex a (Proxy :: Proxy (is:.StateIx I))
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
              let j = getIndex a (Proxy :: Proxy (is:.StateIx I))
              in  TState s a b (ii:.j) (oo:.j) (ee:.()) )
    . termStream ts cs us is
    . staticCheck (sty == E || sty == EL)
    where !sty = styA ! i
  {-# Inline termStream #-}

instance TermStaticVar Epsilon (StateIx t) where
  termStaticVar _ sv _ = sv
  termStreamIndex _ _ i = i
  {-# Inline [0] termStaticVar   #-}
  {-# Inline [0] termStreamIndex #-}



-- * Invisible starting symbol

instance
  ( Monad m
  ) => MkStream m S (StateIx I) where
  mkStream S (IStatic ()) (StateIx _ _ h _) (StateIx cs ty i _)
    = staticCheck (i>=0 && i<=h) . singleton $ ElmS (StateIx cs ty i (-1)) (StateIx cs ty (-1) (-1))
  mkStream S (IVariable ()) (StateIx _ _ h _) (StateIx cs ty i _)
    = filter (const $ 0<=i && i<=h) . singleton $ ElmS (StateIx cs ty i (-1)) (StateIx cs ty (-1) (-1))
  {-# Inline mkStream #-}

instance
  ( Monad m
  , MkStream m S is
  ) => MkStream m S (is:.StateIx I) where
  mkStream S (vs:.IStatic ()) (us:.StateIx _ _ u _) (is:.StateIx c t i _)
    = map (\(ElmS zi zo) -> ElmS (zi:.StateIx c t i (-1)) (zo:.StateIx c t (-1) (-1)))
    . staticCheck (i>=0 && i<=u)
    $ mkStream S vs us is
  mkStream S (vs:.IVariable ()) (us:.StateIx _ _ u _) (is:.StateIx c t i _)
    = map (\(ElmS zi zo) -> ElmS (zi:.StateIx c t i (-1)) (zo:.StateIx c t (-1) (-1)))
    . staticCheck (i>=0 && i<=u)
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
  ( AddIndexDense a us is
  , GetIndex a (is:.StateIx I)
  , GetIx a (is:.StateIx I) ~ (StateIx I)
  ) => AddIndexDense a (us:.StateIx I) (is:.StateIx I) where
  addIndexDenseGo (cs:.c) (vs:.IStatic ()) (us:._) (is:.ix@(StateIx styC styA i _))
    -- makes the assumption that there is a legal @c@hild index coming up.
    -- This should hold (@B@ sets to the second child, for all other rules
    -- the correct child is enumerated).
    -- Note that after this map, no child can be legally selected due to
    -- @(-1)@ in @kt@
    = map (\(SvS s a b tt ii oo) ->
              let kt  = StateIx styC styA (fst $ styC ! (Z:.i:.c)) (-1)   -- the @(fst ...)@ bracket should give us the @c@s child
                  pix = getIndex a (Proxy :: Proxy (is:.StateIx I))
                  c   = _siChild pix
              in  SvS s a b (tt:.kt) (ii:.kt) (oo:.kt) )
    . addIndexDenseGo cs vs us is
  addIndexDenseGo (cs:.c) (vs:.IVariable ()) (us:._) (is:.ix@(StateIx styC styA i _))
    = map (\(SvS s a b tt ii oo) ->
              let kt   = StateIx styC styA (fst $ styC ! (Z:.i:.c)) 1
                  pix  = getIndex a (Proxy :: Proxy (is:.StateIx I))
                  c    = _siChild pix
              in  SvS s a b (tt:.kt) (ii:.kt) (oo:.kt) )
    . addIndexDenseGo cs vs us is
    . staticCheck (stya == B) -- @IVariable@ is only legal in @B@ cases
    where stya = styA ! i
  {-# Inline addIndexDenseGo #-}

instance TableStaticVar (StateIx I) (StateIx I) where
  tableStaticVar   _ _ (IStatic   ()) _ = IVariable ()
  tableStaticVar   _ _ (IVariable ()) _ = IVariable ()
  tableStreamIndex _ _ _ s = s
  {-# Inline [0] tableStaticVar   #-}
  {-# Inline [0] tableStreamIndex #-}

