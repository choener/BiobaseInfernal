
-- | Individual stream functions, to be tested for full fusion.
--
-- TODO Expand BenchmarkHistory to print bytes allocated before / after
-- a function to test for it being tight.

module Main where

import Criterion.Main
import Data.Vector.Fusion.Stream.Monadic as S
import Data.Vector.Fusion.Util
import Data.Vector.Unboxed as VU
import Debug.Trace

import ADP.Fusion
import Biobase.Primary.Letter
import Biobase.Primary.Nuc.RNA
import Data.PrimitiveArray hiding (map, unsafeIndex)

import Biobase.SElab.Bitscore
import Biobase.SElab.CM.ADP.Fusion
import Biobase.SElab.CM.Types hiding (S)
import Biobase.SElab.CM



stream_E_Epsilon :: States -> Int -> Int -> IO Int
stream_E_Epsilon m k l = (f <<< (M:|cme:|cme) % (M:|Epsilon:|Epsilon) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f _ _ = 424242
        h = S.foldl' max 232323
        cme = CMstate (Proxy :: Proxy '["E","EL"])    -- E==7, EL==9
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cme #-}
{-# NoInline stream_E_Epsilon #-}

{-
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
-}

{-
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
-}

{-
stream_MP_MatP_2 :: States -> Int -> Int -> Int
stream_MP_MatP_2 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) ... h) (Z:.mkStateIxH m:.mkStateIxH m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f c e = seq c . seq e $ 424242
        h = S.foldl' max (-232323)
        cmp = cmstateMP -- CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
{-# NoInline stream_MP_MatP_2 #-}
-}

{-
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
-}

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
stream_MP_MatP_5 :: States -> Int -> Int -> Int
stream_MP_MatP_5 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) % (M:|ec:|ec) ... h) (Z:.mkStateIxH m:.mkStateIxH m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f c e1 e2 = seq c . seq e1 . seq e2 $ 424242
        h = S.foldl' max (-232323)
        cmp = cmstateMP
        ec = EmitChar $ VU.fromList acgu
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
{-# NoInline stream_MP_MatP_5 #-}
-}

{-
stream_MP_MatP_6 :: States -> Int -> Int
stream_MP_MatP_6 m k = unId $ (f <<< (M:|cmp) % (M:|ec) % tbl % (M:|ec) ... h) (Z:.mkStateIx0 m) (Z:.mkStateIxAt m k)
  where f !_ !_ !_ !_ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $! VU.fromList acgu
        low = mkStateIx0 m
        high = mkStateIxH m
        tbl :: ITbl Id Unboxed (Z:.EmptyOk) (Z:.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk) (fromAssocs (Z:.low) (Z:.high) (-555555) []) (\_ _ -> return 0 :: Id Bitscore)
        {-# Inline tbl #-}
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_6 #-}
-}

{-
stream_MP_MatP_7 :: States -> Int -> Int -> Int
stream_MP_MatP_7 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|del:|del) ... h) (Z:.mkStateIx0 m:.mkStateIx0 m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f !_ !_ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        del = Deletion
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline del #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_MatP_7 #-}
-}

{-
stream_MP_MatP_8 :: States -> Int -> Int -> Int
stream_MP_MatP_8 m k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) % (M:|ec:|ec) % (M:|ec:|ec) ... h)
                                (Z:.mkStateIxH m:.mkStateIxH m)
                                (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f c (Z:.e11:.e12) (Z:.e21:.e22) (Z:.e31:.e32) = 424242
        h = S.foldl' max (-232323)
        cmp = cmstateMP
        ec = EmitChar $ VU.fromList acgu
        {-# Inline f #-}
        {-# Inline h #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
{-# NoInline stream_MP_MatP_8 #-}
-}

stream_MP_MatP :: States -> Unboxed (Z:.StateIx I :.StateIx I) Bitscore -> Int -> Int -> Int
stream_MP_MatP m tbldata k l = unId $ (f <<< (M:|cmp:|cmp) % (M:|ec:|ec) % tbl % (M:|ec:|ec) ... h) (Z:.mkStateIxH m:.mkStateIxH m) (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f !_ !_ !_ !_ = 424242
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) tbldata (\_ _ -> return 0 :: Id Bitscore)
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

stream_MP_E :: States -> Unboxed (Z:.StateIx I :.StateIx I) Bitscore -> Int -> Int -> Int
stream_MP_E m tbldata k l = unId $ (f_MP <<< (M:|cmp:|cmp) % (M:|ec:|ec) % tbl % (M:|ec:|ec) |||
                                       f_E  <<< (M:|cme:|cme) % (M:|Epsilon:|Epsilon)           ... h
                                      )
                                      (Z:.mkStateIxH m:.mkStateIxH m)
                                      (Z:.mkStateIxAt m k:.mkStateIxAt m l)
  where f_MP !_ !_ !_ !_ = 424242
        f_E  !_ !_ = 434343
        h = S.foldl' max 232323
        cmp = CMstate (Proxy :: Proxy '["MP"])    -- MP==1
        cme = CMstate (Proxy :: Proxy '["E","EL"])    -- E==7, EL==9
        ec = EmitChar $ VU.fromList acgu
        tbl :: ITbl Id Unboxed (Z:.EmptyOk:.EmptyOk) (Z:.StateIx I :.StateIx I) Bitscore
        tbl = ITbl 0 0 (Z:.EmptyOk:.EmptyOk) tbldata (\_ _ -> return 0 :: Id Bitscore)
        low = mkStateIx0 m
        high = mkStateIxH m
        {-# Inline f_MP #-}
        {-# Inline f_E #-}
        {-# Inline cme #-}
        {-# Inline cmp #-}
        {-# Inline ec #-}
        {-# Inline tbl #-}
        {-# Inline low #-}
        {-# Inline high #-}
{-# NoInline stream_MP_E #-}



main :: IO ()
main = do
  [!cm] <- fromFile "tests/test11.cm"
  let !sts  = _states cm
      !tbl  = (fromAssocs (Z:.low:.low) (Z:.high:.high) (-555555) []) :: Unboxed (Z:.StateIx I :.StateIx I) Bitscore
      !low  = mkStateIx0 sts
      !high = mkStateIxH sts
  seq cm . seq sts . seq tbl . seq low . seq high $ defaultMain
    [ bench "E/eps"            $ whnf (\k -> stream_E_Epsilon sts k k) 6   --   3 us
    , bench "MP/cmp/ec/tbl/ec" $ whnf (\k -> stream_MP_MatP sts tbl k k) 6     -- 211 us
    , bench "MP + E"           $ whnf (\k -> stream_MP_E sts tbl k k) 6
    ]

