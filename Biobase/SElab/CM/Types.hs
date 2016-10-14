
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
import           Biobase.SElab.CM.ModelStructure



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







data EntryExit = EntryState | ExitState
  deriving (Eq,Ord,Read,Show,Generic)

{-

-- | Return the main state for a given node.

nodeMainState ee = prism' repack unpack . _3
  where repack :: (Node,Int,State) -> Node
        repack (n,k,s) = n & nstates %~ (VG.// [(k,s)])
        unpack :: Node -> Maybe (Node,Int,State)
        unpack n = (\k -> (n,k,(n^.nstates) VG.! k)) <$> (go >>= extr)
          where go | ty == MatP = Just MP
                   | ty == MatL = Just ML
                   | ty == MatR = Just MR
                   | ty == Bif  && ee == EntryState = Just B
                   | (ty == BegL || ty == BegR) && ee == ExitState  = Just S
                   | otherwise = Nothing
                extr z = VG.findIndex ((==z) . view sType) (n^.nstates)
                ty = n^.ntype



-- | A pure getter to retrieve the last state

sLastState :: Getter States (PInt () StateIndex)
sLastState = sStateType . to bounds . to snd
{-# Inline sLastState #-}

-}



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
  , _cm             :: Either FlexibleModel StaticModel
--  , _nodes          :: V.Vector Node
--  , _states         :: States
  , _hmm            :: HMM Rfam
  , _unknownLines   :: Seq Text
  , _cmIsLocal      :: Bool             -- ^ @True@ if we are in local mode
  }
  deriving (Eq,Show,Read,Generic)

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
    , _cm             = Left def
--    , _nodes          = empty
--    , _states         = def
    , _hmm            = def
    , _unknownLines   = def
    , _cmIsLocal      = False
    }

instance Binary    CM
instance Serialize CM
instance FromJSON  CM
instance ToJSON    CM
instance NFData    CM



-- * Operations on CMs

{-

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
makeLocal cm = (addLocalEnds $ addLocalBegins cm) & cmIsLocal .~ True

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
        addEnds ts = ts `VG.snoc` (elsid,lep)
        lendcm = (cm & nodes %~ (`VG.snoc` ell))
               & nodes.traverse.(nodeMainState ExitState).filtered (not . hasEndNext cm).transitions %~ addEnds

-- |

localEndBitscore :: CM -> Bitscore
localEndBitscore cm = prob2Score 1 $ cm^.pend / (genericLength $ cm^..nodes.folded.(nodeMainState ExitState).filtered (not . hasEndNext cm))

-- |

makeGlobal :: CM -> CM
makeGlobal = undefined

-}

