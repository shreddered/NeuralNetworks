module Main where

import Data.Neuron

main :: IO ()
main = do
  let thresholdAF = ActivationFunction
          { primary  = \x -> if x < 0 then 0 else 1
          , derivative = const 1
          }
      singmoidalAF = ActivationFunction
          { primary = (/ 2) . (+ 1) . tanh
          , derivative = (/ 2) . (^ (-2)) . cosh
          }
      func = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1 ]
      weights = train func 0.3 thresholdAF
  print weights
