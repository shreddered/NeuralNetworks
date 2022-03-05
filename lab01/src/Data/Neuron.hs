module Data.Neuron
    ( ActivationFunction (..)
    -- , Neuron
    , train
    ) where

import Data.Vector (Vector)
import Data.Vector as Vector

-- for now I will keep it simple
data ActivationFunction = ActivationFunction
    { f  :: Double -> Double
    , f' :: Double -> Double
    }

-- data Neuron = Neuron { learningRate :: Double
                     -- , activationFunction :: ActivationFunction
                     -- , weights :: Matrix Double
                     -- }

-- train function
train :: Vector Double      -- reference
      -> Double             -- learning rate
      -> ActivationFunction -- activation function
      -> Vector Double      -- final weights
train = trainHelper (Vector.replicate 4 0, 1) -- a dirty hack

-- train helper (epoch loop)
trainHelper :: (Vector Double, Double) -- (weights, error)
            -> Vector Double           -- reference (boolean function)
            -> Double                  -- learning rate
            -> ActivationFunction      -- activation function
            -> Vector Double           -- final weights
trainHelper (weights, _) _ _ _ = weights
-- trainHelper (weights, err) func lr af = trainHelper (weights', err')
