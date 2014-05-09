{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Biobase.SElab.Bitscore where

import Data.Default.Class
import Data.Primitive.Types
import Data.Vector.Generic as VG
import Data.Vector.Generic.Mutable as VGM
import Data.Vector.Unboxed as VU
import Data.Vector.Unboxed.Base
import Data.Vector.Unboxed.Deriving



-- | Infernal bit score. Behaves like a double (deriving Num).
--
-- Infernal users guide, p.42: log-odds score in log_2 (aka bits).
--
-- S = log_2 (P(seq|CM) / P(seq|null))
--
-- TODO use logfloat, instead of rolling our own (actually maybe not:
-- 'BitScore's are exactly that: log-scaled scores where we expect @(+)@ to add
-- the scores, i.e. we'd multiply in normal space; while LogFloat's act just
-- like floats, but internally they handle everything in log-space).

newtype Bitscore = Bitscore {rawBitscore :: Double}
  deriving (Eq,Ord,Read,Show,Num)

derivingUnbox "Bitscore"
  [t| Bitscore -> Double |] [| rawBitscore |] [| Bitscore |]

-- | A default bitscore of "-infinity".
--
-- TODO Check out the different "defaults" Infernal uses

instance Default Bitscore where
  def = Bitscore (-999999)

-- | Given a null model and a probability, calculate the corresponding
-- 'BitScore'.

prob2Score :: Double -> Double -> Bitscore
prob2Score null x
  | x==0      = def
  | otherwise = Bitscore $ log (x/null) / log 2
{-# INLINE prob2Score #-}

-- | Given a null model and a 'BitScore' return the corresponding probability.

score2Prob :: Double -> Bitscore -> Double
score2Prob null (Bitscore x)
  | x<= -99999 = 0
  | otherwise  = null * exp (x * log 2)
{-# INLINE score2Prob #-}

-- | A simple alias for e-values.
--
-- TODO newtype this?

type Evalue = Double

