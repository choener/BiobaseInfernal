
module Main where

import Criterion.Main
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

