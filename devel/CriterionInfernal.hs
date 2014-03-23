
module Main where

import Numeric.Log
import Criterion.Main

import Biobase.SElab.UnsafeNumeric



main = defaultMain
  [ bgroup "+"  [ bench "Log:Num" $ nf ((+) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (add (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns:alternativeAdd" $ nf (add (Exp (1::Double))) (Exp 2)
                ]                                            
  , bgroup "*"  [ bench "Log:Num" $ nf ((*) (Exp (1::Double))) (Exp 2)
                , bench "Log:Uns" $ nf (mul (Exp (1::Double))) (Exp 2)
                ]
  ]

