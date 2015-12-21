
-- | Individual stream functions, to be tested for full fusion.
--
-- TODO Expand BenchmarkHistory to print bytes allocated before / after
-- a function to test for it being tight.

module Main where

import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Unboxed as VU
import Data.Vector.Fusion.Util

import ADP.Fusion
import Data.PrimitiveArray hiding (map, unsafeIndex)
import Biobase.Primary.Letter
import Biobase.Primary.Nuc.RNA

import Biobase.SElab.CM.ADP.Fusion
import Biobase.SElab.CM.Types hiding (S)
import Biobase.SElab.Bitscore



{-
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

stream_Pass_Epsilon :: States -> Int -> Int -> IO Int
stream_Pass_Epsilon m k l = (f <<< (M:|cme:|cme) % (M:|Epsilon:|Epsilon) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cme = CMstate (Proxy :: Proxy '["MP","->"])    -- MP==1
{-# NoInline stream_Pass_Epsilon #-}

stream_MP_MatP_1 :: States -> Int -> Int -> Int
stream_MP_MatP_1 m k l = unId $ (f <<< (M:|cmp:|cmp) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_1 #-}

stream_MP_MatP_2 :: States -> Int -> Int -> Int
stream_MP_MatP_2 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.low:.low) (Z:.high:.high) (-555555) []) (\_ _ -> return 0 :: Id Bitscore)
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline tbl #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_2 #-}
-}

stream_MP_MatP_3 :: States -> Int -> Int -> Int
stream_MP_MatP_3 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) % tbl ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ _ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.low:.low) (Z:.high:.high) (-555555) []) (\_ _ -> return 0 :: Id Bitscore)
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline tbl #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_3 #-}

{-
stream_MP_MatP_4 :: States -> Int -> Int -> Int
stream_MP_MatP_4 m k l = unId $ (f <<< (M:|cmp:|cmp) % tbl ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.low:.low) (Z:.high:.high) (-555555) []) (\_ _ -> return 0 :: Id Bitscore)
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline tbl #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_4 #-}
-}

{-
stream_MP_MatP :: States -> Int -> Int -> Int
stream_MP_MatP m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) % tbl % (M:|ec:|ec) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ _ _ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) (fromAssocs (Z:.low:.low) (Z:.high:.high) (-555555) []) (\_ _ -> return 0 :: Id Bitscore)
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline tbl #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP #-}
-}



main :: IO ()
main = return ()

