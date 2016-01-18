
-- | Efficient encoding of @Infernal 1.1@ covariance models.

module Biobase.SElab.CM.Types where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Default
import           Data.Hashable (Hashable)
import           Data.Ix (Ix)
import           Data.List (genericLength)
import           Data.Sequence (Seq)
import           Data.Serialize (Serialize)
import           Data.Text (Text)
import           Data.Vector.Generic (empty,fromList,toList)
import           Data.Vector.Generic.Lens
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import           Text.Read
import           Debug.Trace

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Accession
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray hiding (fromList,toList)

import           Biobase.SElab.HMM.Types (HMM)



-- | Extended CM information to calculate e-values

data EValueParams = EValueParams
  { _lambda  :: !Double  -- ^ λ>0 (lambda, slope) for exponential tails for local scores
  , _tau     :: !Double  -- ^ τ (tau, location) for exponential tails for local scores
  , _tau2    :: !Double  -- ^ τ2 (tau, location again) for full histogram of all hits
  , _dbSize  :: !Int     -- ^ database size in residues
  , _numHits :: !Int     -- ^ total number of non-overlapping hits
  , _tailFit :: !Double  -- ^ high-scoring tail fit
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''EValueParams
makePrisms ''EValueParams

instance Default EValueParams where
  def = EValueParams
    { _lambda  = 0
    , _tau     = 0
    , _tau2    = 0
    , _dbSize  = 0
    , _numHits = 0
    , _tailFit = 0
    }

instance Binary    EValueParams
instance Serialize EValueParams
instance FromJSON  EValueParams
instance ToJSON    EValueParams
instance NFData    EValueParams
instance Hashable  EValueParams



-- | The type of a node, efficiently encoded as an Int.

newtype NodeType = NodeType Int
  deriving (Eq,Ord,Generic,Ix)

pattern Bif  = NodeType 0
pattern MatP = NodeType 1
pattern MatL = NodeType 2
pattern MatR = NodeType 3
pattern BegL = NodeType 4
pattern BegR = NodeType 5
pattern Root = NodeType 6
pattern End  = NodeType 7

instance Binary    NodeType
instance FromJSON  NodeType
instance Hashable  NodeType
instance Serialize NodeType
instance ToJSON    NodeType
instance NFData    NodeType

instance Show NodeType where
  show = \case
    Bif  -> "BIF"
    MatP -> "MATP"
    MatL -> "MATL"
    MatR -> "MATR"
    BegL -> "BEGL"
    BegR -> "BEGR"
    Root -> "ROOT"
    End  -> "END"

instance Read NodeType where
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "BIF"  -> Bif
      "MATP" -> MatP
      "MATL" -> MatL
      "MATR" -> MatR
      "BEGL" -> BegL
      "BEGR" -> BegR
      "ROOT" -> Root
      "END"  -> End
      _      -> error $ "read NodeType: " ++ s

derivingUnbox "NodeType"
  [t| NodeType -> Int |] [| \(NodeType n) -> n |] [| NodeType |]

-- | Type of a state, a newtype wrapper for performance

newtype StateType = StateType Int
  deriving (Eq,Ord,Generic,Ix)

pattern D  = StateType 0
pattern MP = StateType 1
pattern ML = StateType 2
pattern MR = StateType 3
pattern IL = StateType 4
pattern IR = StateType 5
pattern S  = StateType 6
pattern E  = StateType 7
pattern B  = StateType 8
pattern EL = StateType 9

instance Binary    StateType
instance FromJSON  StateType
instance Hashable  StateType
instance Serialize StateType
instance ToJSON    StateType
instance NFData    StateType

instance Show StateType where
  show = \case
    D  -> "D"
    MP -> "MP"
    ML -> "ML"
    MR -> "MR"
    IL -> "IL"
    IR -> "IR"
    S  -> "S"
    E  -> "E"
    B  -> "B"
    EL -> "EL"
    (StateType e) -> "StateType " ++ show e

instance Read StateType where
  readPrec = parens $ do
    Ident s <- lexP
    return $ case s of
      "D"  -> D
      "MP" -> MP
      "ML" -> ML
      "MR" -> MR
      "IL" -> IL
      "IR" -> IR
      "S"  -> S
      "E"  -> E
      "B"  -> B
      "EL" -> EL
      _    -> error $ "read StateType: " ++ s

derivingUnbox "StateType"
  [t| StateType -> Int |] [| \(StateType s) -> s |] [| StateType |]

emitsSingle :: StateType -> Bool
emitsSingle s | s `elem` [ML,MR,IL,IR] = True
              | otherwise              = False
{-# Inline emitsSingle #-}

emitsPair = (==) MP
{-# Inline emitsPair #-}



data StateIndex

type Transitions b = Vector (PInt () StateIndex, b)

-- |

data State = State
  { _sType        :: StateType
  , _sid          :: PInt () StateIndex
  , _sParents     :: (PInt () StateIndex, PInt () StateIndex)
  , _sqdb         :: (Int,Int,Int,Int)
  , _transitions  :: Transitions Bitscore
  , _emissions    :: Vector Bitscore  -- emission order is ACGU or AA,AC,AG,AU, CA,CC,CG,CU, GA,GC,GG,GU, UA,UC,UG,UU
  }
  deriving (Show,Read,Generic)

makeLenses ''State
makePrisms ''State

instance Default State where
  def = State
    { _sType        = StateType (-1)
    , _sid          = -1
    , _sParents     = (-1,-1)
    , _sqdb         = (-1,-1,-1,-1)
    , _transitions  = empty
    , _emissions    = empty
    }

instance Binary    State
instance Serialize State
instance FromJSON  State
instance ToJSON    State
instance NFData    State



data NodeIndex

-- | @Node@s are a high-level structure in covariance models, with each
-- node having one or more states as children. In addition, nodes carry
-- alignment-column based information.
--
-- TODO @_nColL@ and @nColR@ should become @Index 1@ types. We'll do that
-- once we re-activate Stockholm file parsing.

data Node = Node
  { _nstates :: V.Vector State
  , _ntype   :: NodeType
  , _nid     :: PInt () NodeIndex
  , _nColL   :: Int
  , _nColR   :: Int
  , _nConL   :: Char
  , _nConR   :: Char
  , _nRefL   :: Char
  , _nRefR   :: Char
  }
  deriving (Show,Read,Generic)

makeLenses ''Node
makePrisms ''Node

data EntryExit = EntryState | ExitState
  deriving (Eq,Ord,Read,Show,Generic)

-- | Return the main state for a given node.
--
-- TODO If this were lens-like we could actually set the parent!

--nodeMainState :: EntryExit -> Node -> Maybe State
nodeMainState ee = prism' fst go . _2
  where go n = (n,) <$> case n^.ntype of
          MatP -> n^? which MP
          MatL -> n^? which ML
          MatR -> n^? which MR
          Bif  | ee == EntryState -> n^? which B
          BegL | ee == ExitState  -> n^? which S
          BegR | ee == ExitState  -> n^? which S
          _    -> Nothing
        which w = nstates . folded . filtered ((==w) . view sType)

--mumu ee n = (n,) <$> nodeMainState ee n

--ohoh ee = prism' fst (mumu ee) . _2

instance Default Node where
  def = Node
    { _nstates  = empty
    , _ntype    = NodeType (-1)
    , _nid      = -1
    , _nColL    = -1
    , _nColR    = -1
    , _nConL    = '-'
    , _nConR    = '-'
    , _nRefL    = '-'
    , _nRefR    = '-'
    }
  {-# Inline def #-}

instance Binary    Node
instance Serialize Node
instance FromJSON  Node
instance ToJSON    Node
instance NFData    Node



-- | Encode all the information necessary to have *efficient* covariance
-- models.
--
-- Transitions are encoded as a boxed vector of unboxed vectors. The outer
-- boxed vector is indexed by the current state. The inner unboxed vector
-- is indexed by the child number. For each child number we record the
-- target state and transition cost.
--
-- TODO emissions pair/single
-- TODO local / global mode
-- TODO add QDB information here?
--
-- TODO We need to modify how BiobaseXNA encodes RNA sequences (maybe ACGUN)

data States = States
  { _sTransitions     :: ! (V.Vector (Transitions Bitscore))                                    -- ^ Transitions to a state, together with the transition score; unpopulated transitions are set to @-1@.
  , _sPairEmissions   :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA:.Letter RNA) Bitscore)   -- ^ Scores for the emission of a pair
  , _sSingleEmissions :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA) Bitscore)               -- ^ Scores for the emission of a single nucleotide
  , _sStateType       :: ! (Unboxed (PInt () StateIndex) StateType)                             -- ^ Type of the state at the current index
  }
  deriving (Show,Read,Generic)

makeLenses ''States
makePrisms ''States

-- | A pure getter to retrieve the last state

sLastState :: Getter States (PInt () StateIndex)
sLastState = sStateType . to bounds . to snd
{-# Inline sLastState #-}

instance Default States where
  def = States
    { _sTransitions     = empty
    , _sPairEmissions   = fromAssocs (Z:.0:.A:.A) (Z:.0:.A:.A) 0                 []
    , _sSingleEmissions = fromAssocs (Z:.0:.A)    (Z:.0:.A)    0                 []
    , _sStateType       = fromAssocs 0            0            (StateType $ -1)  []
    }

instance Binary    States
instance Serialize States
instance FromJSON  States
instance ToJSON    States
instance NFData    States



-- | Covariance models for @Infernal@ non-coding RNA structures.

data CM = CM
  { _version        :: (Text,Text)
  , _name           :: Text
  , _accession      :: Accession Rfam
  , _description    :: Text
  , _clen           :: Int
  , _statesInModel  :: Int
  , _nodesInModel   :: Int
  , _w              :: Int
  , _referenceAnno  :: Bool                   -- ^ have we picked up reference annotation from @GC RF@ lines in Stockholm? and integrated into match states?
  , _consensusRes   :: Bool                   -- ^ valid consensus residue annotation?
  , _alignColMap    :: Bool                   -- ^ if yes, we have map annotation in the main model annotating which multiple-alignment column a state came from
  , _alph           :: Text
  , _date           :: Text
  , _commandLineLog :: Seq Text
  , _pbegin         :: Double
  , _pend           :: Double
  , _wbeta          :: Double
  , _qdbBeta1       :: Double
  , _qdbBeta2       :: Double
  , _n2Omega        :: Double
  , _n3Omega        :: Double
  , _elseLF         :: Double
  , _nseq           :: Int
  , _effn           :: Double
  , _cksum          :: Word32
  , _nullModel      :: Vector Bitscore
  , _ga             :: Double
  , _tc             :: Double
  , _efp7gf         :: (Double,Double)
  , _ecmlc          :: EValueParams
  , _ecmgc          :: EValueParams
  , _ecmli          :: EValueParams
  , _ecmgi          :: EValueParams
  , _nodes          :: V.Vector Node
  , _states         :: States
  , _hmm            :: HMM Rfam
  , _unknownLines   :: Seq Text
  }
  deriving (Show,Read,Generic)

makeLenses ''CM
makePrisms ''CM

instance Default CM where
  def = CM
    { _version        = ("","")
    , _name           = ""
    , _accession      = ""
    , _description    = ""
    , _clen           = 0
    , _statesInModel  = 0
    , _nodesInModel   = 0
    , _w              = 0
    , _referenceAnno  = False
    , _consensusRes   = False
    , _alignColMap    = False
    , _alph           = ""
    , _date           = ""
    , _commandLineLog = def
    , _pbegin         = 0.05
    , _pend           = 0.05
    , _wbeta          = 0
    , _qdbBeta1       = 0
    , _qdbBeta2       = 0
    , _n2Omega        = 0
    , _n3Omega        = 0
    , _elseLF         = 0
    , _nseq           = 0
    , _effn           = 0
    , _cksum          = 0
    , _nullModel      = empty
    , _ga             = 0
    , _tc             = 0
    , _efp7gf         = (0,0)
    , _ecmlc          = def
    , _ecmgc          = def
    , _ecmli          = def
    , _ecmgi          = def
    , _nodes          = empty
    , _states         = def
    , _hmm            = def
    , _unknownLines   = def
    }

instance Binary    CM
instance Serialize CM
instance FromJSON  CM
instance ToJSON    CM
instance NFData    CM



-- * Operations on CMs

-- | Given an otherwise valid 'CM', build the efficient 'States' system.
--
-- Normally, we will do @CM@ manipulations with the @CM@ itself, which is
-- much more highlevel, but slower due to the data structures used.
-- Building the actual 'States' structure provides a low-level
-- high-performance structure for computations.
--
-- TODO check that @ss@ actually is sorted and dense.

buildStatesFromCM :: CM -> States
buildStatesFromCM cm = States
  { _sTransitions     = fromList [ s^.transitions | s <- ss ]
  , _sPairEmissions   = fromAssocs (Z:.0:.A:.A) (Z:.maxState:.U:.U) def
                      . concatMap (\s -> [((Z:.s^.sid:.n1:.n2),e) | emitsPair (s^.sType), ((n1,n2),e) <- zip ((,) <$> acgu <*> acgu) (toList $ s^.emissions)]) $ ss
  , _sSingleEmissions = fromAssocs (Z:.0:.A) (Z:.maxState:.U) def
                      . concatMap (\s -> [((Z:.s^.sid:.nt),e) | emitsSingle (s^.sType), (nt,e) <- zip acgu (toList $ s^.emissions)] ) $ ss
  , _sStateType       = fromAssocs 0 maxState (StateType $ -1) . Prelude.map ((,) <$> view sid <*> view sType) $ ss
  }
  where maxState = maximum $ ss^..folded.sid
        ss = cm^..nodes.folded.nstates.folded

-- | Determine if any of the children for a state is an @E@ state.

hasEndNext :: CM -> State -> Bool
hasEndNext cm s = any (`elem` ss) kids
  where kids = VG.toList . VG.map fst $ s^.transitions
        ss = cm^..nodes.folded.nstates.folded.filtered ((==E) . view sType).sid

-- | Create a local @CM@.
--
-- A local @CM@ has transitions from the @Root 0@ states to all other @MATP
-- / MP@, @MATL / ML@, @MATR / MR@, @BIF / B@ states, except for the state
-- in @Node 1@, which already is connected. The probabilities are set to
-- @PBEGIN@ or @0.05@ in case @PBEGIN@ is not set, and divided by the
-- number of such states to move to.

makeLocal :: CM -> CM
makeLocal cm = addLocalEnds $ addLocalBegins cm

-- | Given a @CM@, add the necessary transitions to create local
-- beginnings.
--
-- This is done by simply adding additional transitions from the @S 0@
-- state and its companions @IL 1@ and @IR 2@.

addLocalBegins :: CM -> CM
addLocalBegins cm = cm & nodes . vectorIx 0 . nstates . traverse . transitions %~ addbegs
  where lbegs = drop 1 $ cm^..nodes.folded.(nodeMainState EntryState).sid -- the first node already has a main transition from @S 0@.
        lbp = localBeginBitscore cm
        addbegs :: Transitions Bitscore -> Transitions Bitscore
        addbegs ts = ts VG.++ (VG.map (,lbp) $ fromList lbegs)

-- | Calculate the actual local beginnings score.

localBeginBitscore :: CM -> Bitscore
localBeginBitscore cm = prob2Score 1 $ cm^.pbegin / genericLength lbegs
  where lbegs = drop 1 $ cm^..nodes.folded.(nodeMainState EntryState).sid

-- | Add a single @local end@ state and transitions to this state.

addLocalEnds :: CM -> CM
addLocalEnds cm = lendcm & states .~ (buildStatesFromCM lendcm)
  where elsid = maximum (cm^..nodes.folded.nstates.folded.sid) + 1
        el = State    -- the new local end state to be inserted.
              { _sType = EL
              , _sid   = elsid
              , _sParents = (-1,-1) -- TODO ???
              , _sqdb = (0,0,0,0) -- TODO ???
              , _transitions = empty
              , _emissions = empty
              }
        ell = Node
              { _nstates = VG.singleton el
              , _ntype = End
              , _nid = maximum (cm^..nodes.folded.nid) + 1
              , _nColL = -1   -- TODO all below
              , _nColR = -1
              , _nConL = '-'
              , _nConR = '-'
              , _nRefL = '-'
              , _nRefR = '-'
              }
        lep = localEndBitscore cm
        addEnds :: Transitions Bitscore -> Transitions Bitscore
        addEnds ts = traceShow ("x",ts) $ ts `VG.snoc` (elsid,lep)
        lendcm = (cm & nodes %~ (`VG.snoc` ell))
               & nodes.traverse.(nodeMainState ExitState).filtered (not . hasEndNext cm).transitions %~ addEnds

-- |

localEndBitscore :: CM -> Bitscore
localEndBitscore cm = prob2Score 1 $ cm^.pend / (genericLength $ cm^..nodes.folded.(nodeMainState ExitState).filtered (not . hasEndNext cm))

-- |

makeGlobal :: CM -> CM
makeGlobal = undefined

