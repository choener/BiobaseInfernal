
-- | Efficient encoding of @Infernal 1.1@ covariance models.

module Biobase.SElab.CM.Types where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson (FromJSON,ToJSON)
import           Data.Binary (Binary)
import           Data.Default
import           Data.Hashable (Hashable)
import           Data.Ix (Ix)
import           Data.Sequence (Seq)
import           Data.Serialize (Serialize)
import           Data.Text (Text)
import           Data.Vector.Generic (empty)
import           Data.Vector.Unboxed.Deriving
import           Data.Vector.Unboxed (Vector)
import           Data.Word (Word32)
import           GHC.Generics (Generic)
import qualified Data.Vector as V (Vector)
import           Text.Read

import           Biobase.Primary.Letter
import           Biobase.Primary.Nuc.RNA
import           Biobase.Types.Accession
import           Data.PrimitiveArray

import           Biobase.SElab.Bitscore
import           Biobase.SElab.HMM.Types (HMM)
import           Biobase.SElab.Types



-- | Extended CM information to calculate e-values

data EValueParams = EValueParams
  { _lambda  :: Double  -- ^ λ>0 (lambda, slope) for exponential tails for local scores
  , _tau     :: Double  -- ^ τ (tau, location) for exponential tails for local scores
  , _tau2    :: Double  -- ^ τ2 (tau, location again) for full histogram of all hits
  , _dbSize  :: Int     -- ^ database size in residues
  , _numHits :: Int     -- ^ total number of non-overlapping hits
  , _tailFit :: Double  -- ^ high-scoring tail fit
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

-- |

data State = State
  { _sType        :: StateType
  , _sid          :: PInt () StateIndex
  , _sParents     :: (PInt () StateIndex, PInt () StateIndex)
  , _sChildren    :: [PInt () StateIndex] -- , Int)               -- ^ first child and number of children (all consecutive); for 'B', it is first and second child as direct index number
  , _sqdb         :: (Int,Int,Int,Int)
  , _transitions  :: Vector Bitscore
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
    , _sChildren    = []
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
  { _nstates :: Vector (PInt () StateIndex)
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
-- TODO state types
-- TODO transitions and associated costs
-- TODO emissions pair/single
-- TODO local / global mode
-- TODO add QDB information here?
--
-- TODO maybe add @_sNumTransitions@ so that we don't have to check for @-1@.
-- TODO maybe check for @0@ instead of @-1@, there is an ASM op for that
--
-- TODO We need to modify how BiobaseXNA encodes RNA sequences (maybe ACGUN)

data States = States
  { _sTransitions     :: ! (Unboxed (Z:.PInt () StateIndex:.Int) (PInt () StateIndex,Bitscore))   -- ^ Transitions to a state, together with the transition score; unpopulated transitions are set to @-1@.
  , _sPairEmissions   :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA:.Letter RNA) Bitscore)  -- ^ Scores for the emission of a pair
  , _sSingleEmissions :: ! (Unboxed (Z:.PInt () StateIndex:.Letter RNA) Bitscore)              -- ^ Scores for the emission of a single nucleotide
  , _sStateType       :: ! (Unboxed (PInt () StateIndex) StateType)                            -- ^ Type of the state at the current index
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
    { _sTransitions     = fromAssocs (Z:.0:.0)    (Z:.0:.0)    (0,0)             []
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
    , _pbegin         = 0
    , _pend           = 0
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

