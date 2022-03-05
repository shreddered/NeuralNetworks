module Neuron
    ( ActivationFunction
    , Neuron
    , someFunc
    ) where

import Numeric.LinearAlgebra.HMatrix

-- for now I will keep it simple
data ActivationFunction = ActivationFunction { f  :: Double -> Double
                                             , f' :: Double -> Double
                                             }

data Neuron = Neuron { learningRate :: Double
                     , activationFunction :: ActivationFunction
                     , weights :: Matrix Double
                     }

someFunc :: IO ()
someFunc = putStrLn "someFunc"
