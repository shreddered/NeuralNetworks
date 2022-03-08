module Data.Extrapolation
    ( ActivationFunction (..)
    , generatePoints
    ) where

data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    }

-- function for point generation
generatePoints :: Double   -- a
               -> Double   -- b
               -> Int      -- number of points
               -> [Double] -- points
generatePoints a b n = let step = (b - a) / (realToFrac n)
                        in take n $ iterate (+ step) a
