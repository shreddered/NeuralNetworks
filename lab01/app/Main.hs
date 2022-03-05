module Main where

import Data.Neuron

main :: IO ()
main = do
  let thresholdAF = ActivationFunction
          { f  = \x -> if x < 0 then 0 else 1
          , f' = const 1
          }
      singmoidalAF = ActivationFunction
          { f  = (/ 2) . (+ 1) . tanh
          , f' = const 1
          }
  putStrLn "Hello world"
