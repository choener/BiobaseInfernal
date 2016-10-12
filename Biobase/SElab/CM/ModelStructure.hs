
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

import qualified Data.Vector as V
import qualified Data.Vector.Generic as VG
import           GHC.Generics (Generic)
import           Data.Binary (Binary)
import           Data.Default
import           Data.Hashable (Hashable)
import           Data.Serialize (Serialize)
import           Data.Aeson (FromJSON,ToJSON)
import           Control.DeepSeq
import           Data.Ix (Ix)
import           Text.Read
import           Data.Vector.Unboxed.Deriving
import           Control.Lens
import           Data.Set (Set)
import           Data.Map (Map)
import qualified Data.Map as M

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Bitscore
import           Data.PrimitiveArray hiding (fromList,toList)



data NodeIndex

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



data StateIndex

type Transitions b = Vector (PInt () StateIndex, b)

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



-- * High-performance structure for @Node@s.

data Nodes = Nodes
  { _nNodesType   :: ! (Unboxed (PInt () NodeIndex) NodeType)
  , _nNodesStates :: ! (Boxed   (PInt () NodeIndex) (V.Vector (PInt () StateIndex)))
  , _nNodesColL   :: ! (Unboxed (PInt () NodeIndex) Int)
  , _nNodesColR   :: ! (Unboxed (PInt () NodeIndex) Int)
  , _nNodesRefL   :: ! (Unboxed (PInt () NodeIndex) Char)
  , _nNodesRefR   :: ! (Unboxed (PInt () NodeIndex) Char)
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''Nodes
makePrisms ''Nodes

instance Default Nodes where
  def = Nodes
    { _nNodesType   = fromAssocs 0 0 (NodeType $ -1) []
    , _nNodesStates = fromAssocs 0 0 VG.empty        []
    , _nNodesColL   = fromAssocs 0 0 (-1)            []
    , _nNodesColR   = fromAssocs 0 0 (-1)            []
    , _nNodesRefL   = fromAssocs 0 0 '-'             []
    , _nNodesRefR   = fromAssocs 0 0 '-'             []
    }

instance Binary    Nodes
instance Serialize Nodes
instance FromJSON  Nodes
instance ToJSON    Nodes
instance NFData    Nodes




-- * High-performance structure for @State@s. Actual calculations are run
-- on these.

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
  { _sTransitions     :: ! (V.Vector (Transitions Bitscore))
  -- ^ Transitions to a state, together with the transition score;
  -- unpopulated transitions are set to @-1@.
  -- TODO we have "forbidden" transitions. Consider how to handle these.
  -- TODO turn this into a @Boxed (Z:.PInt () StateIndex) (Transitions
  -- Bitscore)@ ?
  , _sPairEmissions   :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA:.Letter RNA) Bitscore)
  -- ^ Scores for the emission of a pair
  , _sSingleEmissions :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA) Bitscore)
  -- ^ Scores for the emission of a single nucleotide
  , _sStateType       :: ! (Unboxed (PInt () StateIndex) StateType)
  -- ^ Type of the state at the current index
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''States
makePrisms ''States

-- | A pure getter to retrieve the last state

sLastState :: Getter States (PInt () StateIndex)
sLastState = sStateType . to bounds . to snd
{-# Inline sLastState #-}

instance Default States where
  def = States
    { _sTransitions     = V.empty
    , _sPairEmissions   = fromAssocs (Z:.0:.A:.A) (Z:.0:.A:.A) 0                 []
    , _sSingleEmissions = fromAssocs (Z:.0:.A)    (Z:.0:.A)    0                 []
    , _sStateType       = fromAssocs 0            0            (StateType $ -1)  []
    }

instance Binary    States
instance Serialize States
instance FromJSON  States
instance ToJSON    States
instance NFData    States



-- * Nodes for dynamically changeable models.

-- | @Node@s are a high-level structure in covariance models, with each
-- node having one or more states as children. In addition, nodes carry
-- alignment-column based information.
--
-- TODO @_nColL@ and @nColR@ should become @Index 1@ types. We'll do that
-- once we re-activate Stockholm file parsing.

data Node = Node
  { _nType   :: ! NodeType
  -- ^ Type of the node
  , _nStates :: ! (V.Vector (PInt () StateIndex))
  -- ^ States associated with this node
  , _nColL   :: ! Int
  -- ^ Column index in the corresponding Stockholm file
  , _nColR   :: ! Int
  -- ^ Column index in the corresponding Stockholm file
  , _nConL   :: ! Char
  -- ^ TODO
  , _nConR   :: ! Char
  -- ^ TODO
  , _nRefL   :: ! Char
  -- ^ TODO
  , _nRefR   :: ! Char
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



-- | A single state in a model.
--
-- TODO Map (PInt () StateIndex) State

data State = State
  { _sType        :: ! StateType
    -- ^ The type of the current state
  , _sParents     :: ! (Vector (PInt () StateIndex))
    -- ^ List of parents into this state
  , _sqdb         :: ! QDB
    -- ^ QDB information
  , _transitions  :: ! (Transitions Bitscore)
    -- ^ Into which children to we transition to
  , _emissions    :: ! (Vector Bitscore)
    -- ^ Finally, emission scores, if given for this state. Different
    -- stochastic models should interpret this differently!
    -- For covariance models, the emission order is ACGU for single states
    -- or AA,AC,AG,AU, CA,CC,CG,CU, GA,GC,GG,GU, UA,UC,UG,UU for pair
    -- states.
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''State
makePrisms ''State

instance Default State where
  def = State
    { _sType        = StateType (-1)
    , _sParents     = VG.empty
    , _sqdb         = def
    , _transitions  = VG.empty
    , _emissions    = VG.empty
    }

instance Binary    State
instance Serialize State
instance FromJSON  State
instance ToJSON    State
instance NFData    State



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

