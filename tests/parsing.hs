
-- | We benchmark different stages of parsing. @small@, @medium@, and
-- @large@ are three sets of CMs with different file sizes.
--
-- TODO
--
-- - only extract header information
-- - single-threaded full parsing
-- - multi-threaded full parsing

module Main where

import Criterion.Main


main :: IO ()
main = defaultMain
  [ bgroup "header"
    [ bench "small"  $ nfIO (return ())
    , bench "medium" $ nfIO (return ())
    , bench "large"  $ nfIO (return ())
    ]
  , bgroup "singlethreaded"
    [ bench "small"  $ nfIO (return ())
    , bench "medium" $ nfIO (return ())
    , bench "large"  $ nfIO (return ())
    ]
  , bgroup "multithreaded"
    [ bench "small"  $ nfIO (return ())
    , bench "medium" $ nfIO (return ())
    , bench "large"  $ nfIO (return ())
    ]
  ]


{-
import Numeric.Log

import Biobase.SElab.UnsafeNumeric



main = defaultMain
  -- performance of save vs. unsafe addition in log-space
  [ bgroup "+"  [ bench "Log:Num" $ nf ((+) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (add (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns:alternativeAdd" $ nf (add (Exp (1::Double))) (Exp 2)
                ]
  -- performance of save vs. unsafe multiplication in log-space
  , bgroup "*"  [ bench "Log:Num" $ nf ((*) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (mul (Exp (1::Double))) (Exp 2)
                ]
  ]
-}

