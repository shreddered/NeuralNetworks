module Data.BackpropNet
    ( BackpropNet
    , Layer
    ) where

import Numeric.LinearAlgebra.Data

data ActivationFunc = ActivationFunc
    { primary :: Double -> Double
    , derivative :: Double -> Double
    }

data Layer = Layer
    { activationFunc :: ActivationFunc
    , weights :: Matrix Double
    }

-- for now I will keep it simple
data BackpropNet = BackpropNet
    { firstLayer :: Layer
    , secondLayer :: Layer
    , learningRate :: Double
    }

backpropNet :: Vector Double   -- input vector
            -> Vector Double   -- output vector
            -> Double          -- learning rate
            -> (Int, Int, Int) -- ~~btw i use~~ arch
            -> BackpropNet
backpropNet x t learningRate (n, j, m) = 
