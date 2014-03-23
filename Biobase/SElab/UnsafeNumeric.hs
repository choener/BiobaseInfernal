{-# LANGUAGE BangPatterns #-}

module Biobase.SElab.UnsafeNumeric where

import Numeric.Log
import Control.Applicative



-- * Unsafe 'Num' and 'Fractional' operations. Safe operations involve checks
-- like testing for @Infinity@, which can seriously degrade performance in
-- tight inner loops.

-- | A class of unsafe operations mirroring the 'Num' type class.

class UnsafeNum a where
  mul :: a -> a -> a
  add :: a -> a -> a
  sub :: a -> a -> a

infixl 7 `mul`
infixl 6 `add`
infixl 6 `sub`

-- | Unsafe fractional operations.

class UnsafeFractional a where
  dvd :: a -> a -> a

infixl 7 `dvd`

-- | Unsafe numerical operations for 'Log' types.

instance (Ord a, Num a, Precise a) => UnsafeNum (Log a) where
  mul (Exp a) (Exp b) = Exp $ a+b
  add (Exp a) (Exp b)
    | a >= b    = Exp $ a + log1p (exp $ b - a)
    | otherwise = Exp $ b + log1p (exp $ a - b)
  sub (Exp a) (Exp b) = Exp $ a + log1p (negate $ exp $ b - a)
  {-# INLINE mul #-}
  {-# INLINE add #-}
  {-# INLINE sub #-}

alternativeAdd :: (Ord a, Num a, Precise a) => Log a -> Log a -> Log a
alternativeAdd (Exp a) (Exp b) = let !ma = max a b; !mi = min a b in Exp $ ma + log1p (exp $ mi - ma)
{-# INLINE alternativeAdd #-}

-- | Unsafe fractional operations for 'Log' types.

instance (Fractional a) => UnsafeFractional (Log a) where
  dvd (Exp a) (Exp b) = Exp $ a-b
  {-# INLINE dvd #-}

