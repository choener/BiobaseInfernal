
-- | Defines two model structures. One structure is designed to be easily
-- modifiable for working with a CM. The second is "static" but efficient
-- to use in applications. An isomorphism between the two structures is
-- provided.
--
-- TODO Generalize to both, HMMs and CMs. This will require some thinking
-- on how to generalize everything from individual states to emission
-- systems. Emissions can probably be phantom-typed so that we know
-- emission orders, and other things.

module Biobase.SElab.CM.ModelStructure where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Default
import           Data.Hashable (Hashable)
import           Data.Ix (Ix)
import           Data.Map (Map)
import           Data.Serialize (Serialize)
import           Data.Set (Set)
import           Data.Vector.Unboxed.Deriving
import           GHC.Generics (Generic)
import qualified Data.Map as M
import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Unboxed as VU
import           Text.Read

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray hiding (fromList,toList,map)



-- * General things

-- | Phantom-type a node index of @PInt@s

data NodeIndex

-- | Phantom-type a state index @PInt@s

data StateIndex

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



-- * QDB parameters.

-- | Query-dependent banding parameters. The four parameters are given in
-- increasing order. They are set to @-1@ if not given.

data QDB = QDB
  { _minExpSeqLenBeta2 :: ! Int
  , _minExpSeqLenBeta1 :: ! Int
  , _maxExpSeqLenBeta1 :: ! Int
  , _maxExpSeqLenBeta2 :: ! Int
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''QDB
makePrisms ''QDB

instance Default QDB where
  def = QDB
    { _minExpSeqLenBeta2 = -1
    , _minExpSeqLenBeta1 = -1
    , _maxExpSeqLenBeta1 = -1
    , _maxExpSeqLenBeta2 = -1
    }

instance Binary    QDB
instance Serialize QDB
instance FromJSON  QDB
instance ToJSON    QDB
instance NFData    QDB

derivingUnbox "QDB"
  [t| QDB -> (Int,Int,Int,Int) |] [| \(QDB a b c d) -> (a,b,c,d) |] [| \(a,b,c,d) -> QDB a b c d |]



type Transitions b = VU.Vector (PInt () StateIndex, b)



-- | A single state in a model.
--
-- TODO Map (PInt () StateIndex) State

data State = State
  { _stateType        :: ! StateType
    -- ^ The type of the current state
  , _stateParents     :: ! (VU.Vector (PInt () StateIndex))
    -- ^ List of parents into this state
  , _stateQDB         :: ! QDB
    -- ^ QDB information
  , _stateTransitions :: ! (Transitions Bitscore)
    -- ^ Into which children to we transition to
  , _stateEmissions   :: ! (VU.Vector Bitscore)
    -- ^ Finally, emission scores, if given for this state. Different
    -- stochastic models should interpret this differently!
    -- For covariance models, the emission order is ACGU for single states
    -- or AA,AC,AG,AU, CA,CC,CG,CU, GA,GC,GG,GU, UA,UC,UG,UU for pair
    -- states.
    -- TODO really only one entry?
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''State
makePrisms ''State

instance Default State where
  def = State
    { _stateType        = StateType (-1)
    , _stateParents     = VG.empty
    , _stateQDB         = def
    , _stateTransitions = VG.empty
    , _stateEmissions   = VG.empty
    }

instance Binary    State
instance Serialize State
instance FromJSON  State
instance ToJSON    State
instance NFData    State



-- * High-performance structure for @State@s. Actual calculations are run
-- on these.

-- | Encode all the information necessary to have *efficient* covariance
-- models.
--
-- The index @PInt () StateIndex@ is the actual index type as given in
-- a model description.
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
--
-- TODO ugly but more efficient? Use just a single @Emit@ data structure?

data States = States
  { _statesType        :: ! (Unboxed (PInt () StateIndex) StateType)
  -- ^ Type of the state at the current index
  , _statesParents     :: ! (Boxed (PInt () StateIndex) (VU.Vector (PInt () StateIndex)))
  -- ^ For each state, record which other states lead here
  , _statesTransitions :: ! (Boxed (PInt () StateIndex) (Transitions Bitscore))
  -- ^ Transitions to a state, together with the transition score;
  -- unpopulated transitions are set to @-1@.
  -- TODO we have "forbidden" transitions. Consider how to handle these.
  -- Easy solution is very low bitscores, maybe @-neginf@?
  , _statesQDB          :: ! (Unboxed (PInt () StateIndex) QDB)
  , _statesEmitPair    :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA:.Letter RNA) Bitscore)
  -- ^ Scores for the emission of a pair
  , _statesEmitSingle  :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA) Bitscore)
  -- ^ Scores for the emission of a single nucleotide
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''States
makePrisms ''States

instance Default States where
  def = States
    { _statesType        = fromAssocs 0            0            (StateType $ -1)  []
    , _statesParents     = fromAssocs 0            0            VG.empty          []
    , _statesTransitions = fromAssocs 0            0            VG.empty          []
    , _statesQDB         = fromAssocs 0            0            def               []
    , _statesEmitPair    = fromAssocs (Z:.0:.A:.A) (Z:.0:.A:.A) 0                 []
    , _statesEmitSingle  = fromAssocs (Z:.0:.A)    (Z:.0:.A)    0                 []
    }

instance Binary    States
instance Serialize States
instance FromJSON  States
instance ToJSON    States
instance NFData    States

-- | A pure getter to retrieve the last state

sLastState :: Getter States (PInt () StateIndex)
sLastState = statesType . to bounds . to snd
{-# Inline sLastState #-}



-- * Nodes for dynamically changeable models.

-- | @Node@s are a high-level structure in covariance models, with each
-- node having one or more states as children. In addition, nodes carry
-- alignment-column based information.
--
-- TODO @_nColL@ and @nColR@ should become @Index 1@ types. We'll do that
-- once we re-activate Stockholm file parsing.

data Node = Node
  { _nodeType   :: ! NodeType
  -- ^ Type of the node
  , _nodeStates :: ! (V.Vector (PInt () StateIndex))
  -- ^ States associated with this node
  , _nodeColL   :: ! Int
  -- ^ Column index in the corresponding Stockholm file
  , _nodeColR   :: ! Int
  -- ^ Column index in the corresponding Stockholm file
  , _nodeConL   :: ! Char
  -- ^ TODO
  , _nodeConR   :: ! Char
  -- ^ TODO
  , _nodeRefL   :: ! Char
  -- ^ TODO
  , _nodeRefR   :: ! Char
  -- ^ TODO
  }
  deriving (Eq,Ord,Show,Read,Generic)

makeLenses ''Node
makePrisms ''Node

instance Binary    Node
instance Serialize Node
instance FromJSON  Node
instance ToJSON    Node
instance NFData    Node



-- * High-performance structure for @Node@s.

data Nodes = Nodes
  { _nodesType   :: ! (Unboxed (PInt () NodeIndex) NodeType)
  , _nodesStates :: ! (Boxed   (PInt () NodeIndex) (V.Vector (PInt () StateIndex)))
  , _nodesColL   :: ! (Unboxed (PInt () NodeIndex) Int)
  , _nodesColR   :: ! (Unboxed (PInt () NodeIndex) Int)
  , _nodesConL   :: ! (Unboxed (PInt () NodeIndex) Char)
  , _nodesConR   :: ! (Unboxed (PInt () NodeIndex) Char)
  , _nodesRefL   :: ! (Unboxed (PInt () NodeIndex) Char)
  , _nodesRefR   :: ! (Unboxed (PInt () NodeIndex) Char)
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''Nodes
makePrisms ''Nodes

instance Default Nodes where
  def = Nodes
    { _nodesType   = fromAssocs 0 0 (NodeType $ -1) []
    , _nodesStates = fromAssocs 0 0 VG.empty        []
    , _nodesColL   = fromAssocs 0 0 (-1)            []
    , _nodesColR   = fromAssocs 0 0 (-1)            []
    , _nodesConL   = fromAssocs 0 0 '-'             []
    , _nodesConR   = fromAssocs 0 0 '-'             []
    , _nodesRefL   = fromAssocs 0 0 '-'             []
    , _nodesRefR   = fromAssocs 0 0 '-'             []
    }

instance Binary    Nodes
instance Serialize Nodes
instance FromJSON  Nodes
instance ToJSON    Nodes
instance NFData    Nodes




data StaticModel = StaticModel
  { _smStates :: ! States
  , _smNodes  :: ! Nodes
  }
  deriving (Eq,Show,Read,Generic)

instance Binary    StaticModel
instance Serialize StaticModel
instance FromJSON  StaticModel
instance ToJSON    StaticModel
instance NFData    StaticModel

instance Default StaticModel where
  def = StaticModel
    { _smStates = def
    , _smNodes  = def
    }

-- | Model structure that is somewhat easy to modify. Before turning this
-- into a @StaticModel@, the model itself needs to be valid.

data FlexibleModel = FlexibleModel
  { _fmStates :: ! (Map (PInt () StateIndex) State)
  , _fmNodes  :: ! (Map (PInt () NodeIndex ) Node )
  }
  deriving (Eq,Show,Read,Generic)

instance Binary    FlexibleModel
instance Serialize FlexibleModel
instance FromJSON  FlexibleModel
instance ToJSON    FlexibleModel
instance NFData    FlexibleModel

instance Default FlexibleModel where
  def = FlexibleModel
    { _fmStates = def
    , _fmNodes  = def
    }



isValidModel :: FlexibleModel -> Bool
isValidModel = error "isvalidModel: write me!"



-- * Isomorphisms between static and flexible models
--
-- @flexibleToStatic . staticToFlexible == id@
-- @staticToFlexible . flexibleToStatic == id@

-- | Make a flexible model static.
--
-- TODO should *really* do some basic tests
--
-- TODO this would be easier if we introduced hybrid arrays, not just
-- @Unboxed@ and @Boxed@. ... or if everything were unboxed.
--
-- TODO needs to generalize over the actual model we are dealing with. This
-- includes how many characters to emit in pair and single. And the
-- underlying alphabet.
--
-- TODO use @isValidModel@ for tests.

flexibleToStatic :: FlexibleModel -> StaticModel
flexibleToStatic (FlexibleModel s n)
  | True = StaticModel s' n'
  where s' = States
          { _statesType        = fromAssocs 0 mix (StateType $ -1) $ zip ix $ s ^.. traverse . stateType
          , _statesParents     = fromAssocs 0 mix VG.empty         $ zip ix $ s ^.. traverse . stateParents
          , _statesTransitions = fromAssocs 0 mix VG.empty         $ zip ix $ s ^.. traverse . stateTransitions
          , _statesQDB         = fromAssocs 0 mix def              $ zip ix $ s ^.. traverse . stateQDB
          --
          , _statesEmitPair    = fromAssocs (Z:.0:.A:.A) (Z:.mix:.U:.U) def $
                [ (Z:.k:.n1:.n2,e)
                | (k,es) <- zip ix $ s ^.. traverse . stateEmissions
                , VG.length es == 16
                , ((n1,n2),e) <- zip ((,) <$> acgu <*> acgu) (VG.toList es)
                ]
          , _statesEmitSingle  = fromAssocs (Z:.0:.A) (Z:.mix:.U) def $
                [ (Z:.k:.n1,e)
                | (k,es) <- zip ix $ s ^.. traverse . stateEmissions
                , VG.length es == 4
                , ((n1),e) <- zip acgu (VG.toList es)
                ]
          } where ix = M.keys s ; mix = maximum ix
        n' = Nodes
          { _nodesType   = fromAssocs 0 mix (NodeType $ -1) $ zip ix $ n ^.. traverse . nodeType
          , _nodesStates = fromAssocs 0 mix VG.empty        $ zip ix $ n ^.. traverse . nodeStates
          , _nodesColL   = fromAssocs 0 mix (-1)            $ zip ix $ n ^.. traverse . nodeColL
          , _nodesColR   = fromAssocs 0 mix (-1)            $ zip ix $ n ^.. traverse . nodeColR
          , _nodesConL   = fromAssocs 0 mix '-'             $ zip ix $ n ^.. traverse . nodeConL
          , _nodesConR   = fromAssocs 0 mix '-'             $ zip ix $ n ^.. traverse . nodeConR
          , _nodesRefL   = fromAssocs 0 mix '-'             $ zip ix $ n ^.. traverse . nodeRefL
          , _nodesRefR   = fromAssocs 0 mix '-'             $ zip ix $ n ^.. traverse . nodeRefR
          } where ix = M.keys n ; mix = maximum ix

-- | Make static model flexible again.
--
-- Static models are always (defined to be) valid models.
--
-- TODO emission handling for generalized models

staticToFlexible :: StaticModel -> FlexibleModel
staticToFlexible (StaticModel States{..} Nodes{..})
  = FlexibleModel s' n'
  where s' = M.fromList $ map goS $ uncurry enumFromTo $ bounds _statesType
        n' = M.fromList $ map goN $ uncurry enumFromTo $ bounds _nodesType
        goS k = (k,) $ State
          { _stateType        = t
          , _stateParents     = _statesParents      ! k
          , _stateQDB         = _statesQDB          ! k
          , _stateTransitions = _statesTransitions  ! k
          , _stateEmissions   = if | emitsPair   t -> VG.fromList [ _statesEmitPair   ! (Z:.k:.i:.j) | (i,j) <- (,) <$> acgu <*> acgu ]
                                   | emitsSingle t -> VG.fromList [ _statesEmitSingle ! (Z:.k:.i   ) | i <- acgu ]
                                   | otherwise     -> VG.empty
          } where t = _statesType ! k
        goN k = (k,) $ Node
          { _nodeType   = _nodesType   ! k
          , _nodeStates = _nodesStates ! k
          , _nodeColL   = _nodesColL   ! k
          , _nodeColR   = _nodesColR   ! k
          , _nodeConL   = _nodesConL   ! k
          , _nodeConR   = _nodesConR   ! k
          , _nodeRefL   = _nodesRefL   ! k
          , _nodeRefR   = _nodesRefR   ! k
          }

{-

-- *

-- |

data ModelStruct
  -- | Static, state-based structure. Highly efficient, but not friendly
  -- for modifications.
  = Static
    { _sStates  :: ! States
    -- ^ We only need the states for the actual computations.
    , _sNodes   :: ! Nodes
    -- ^ Nodes are kept as well, but are not used (typically).
    }
  | Dynamic
    { _dStates :: ! (Map (PInt () StateIndex) State)
    , _dNodes  :: ! (Map (PInt () NodeIndex ) Node )
    }
  deriving (Eq,Show,Read,Generic)

makeLenses ''ModelStruct
makePrisms ''ModelStruct

instance Binary    ModelStruct
instance Serialize ModelStruct
instance FromJSON  ModelStruct
instance ToJSON    ModelStruct
instance NFData    ModelStruct
-}

