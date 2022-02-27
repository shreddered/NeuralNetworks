module Main where

import Data.Vector

type Matrix a = Vector (Vector a)

data ActivationFunction = ActivationFunction { f :: Double -> Double
                                             , f' :: Double -> Double
                                             }

data Layer = Layer { weights            :: Matrix Double
                   , activationFunction :: ActivationFunction
                   }

data NeuralNetwork = NeuralNetwork { layers       :: [Layer]
                                   , learningRate :: Double
                                   }

train :: NeuralNetwork -> Vector Double -> NeuralNetwork
train neuralNetwork = _a

main :: IO ()
main = putStrLn "Hello, Haskell!"
