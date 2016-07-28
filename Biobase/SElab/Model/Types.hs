
-- | Generic model wrapper.

module Biobase.SElab.Model.Types where

import           GHC.Generics (Generic)
import           Data.Text (Text)
import           Control.Lens

import           Biobase.SElab.CM.Types (CM)
import           Biobase.SElab.HMM.Types (HMM)
import qualified Biobase.SElab.CM.Types as CM
import qualified Biobase.SElab.HMM.Types as HMM



-- | Generic model wrapper.

data Model = Model
  { _model :: Either (HMM ()) CM
  }
  deriving (Eq,Show,Read,Generic)

makeLenses ''Model

-- | A getter for the name of the model. Be it CM or HMM.

modelName :: Getter Model Text
modelName = to f
  where f m = case _model m of
                Left hmm -> HMM._name hmm
                Right cm -> CM._name cm

