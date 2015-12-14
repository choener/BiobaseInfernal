
-- | Individual stream functions, to be tested for full fusion.
--
-- TODO Expand BenchmarkHistory to print bytes allocated before / after
-- a function to test for it being tight.

module Main where

import Data.Vector.Fusion.Stream.Monadic as S

import ADP.Fusion
import Data.PrimitiveArray hiding (map, unsafeIndex)

import Biobase.SElab.CM.ADP.Fusion
import Biobase.SElab.CM.Types hiding (S)



stream_E_Epsilon :: States -> Int -> Int -> IO Int
stream_E_Epsilon m k l = (f <<< (M:|cme:|cme) % (M:|Epsilon:|Epsilon) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cme = CMstate (Proxy :: Proxy '["E","EL"])    -- E==7, EL==9
{-# NoInline stream_E_Epsilon #-}

stream_MP_Epsilon :: States -> Int -> Int -> IO Int
stream_MP_Epsilon m k l = (f <<< (M:|cme:|cme) % (M:|Epsilon:|Epsilon) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cme = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
{-# NoInline stream_MP_Epsilon #-}



main :: IO ()
main = return ()

