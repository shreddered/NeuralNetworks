module Data.Extrapolation
    ( ActivationFunction (..)
    , generatePoints
    ) where

data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    }

-- function for point generation
generatePoints :: (Double, Double) -- (a, b)
               -> Int              -- number of points
               -> [Double]         -- points
generatePoints (a, b) n = let step = (b - a) / (realToFrac n)
                           in take n $ iterate (+ step) a

train :: Double             -- learning rate
      -> [Double]           -- points
      -> Int                -- number of epochs
      -> Int                -- size of window
      -> (Double, [Double]) -- (epsilon, weights)
train learningRate points n windowSize =
    foldl foo (0, replicate (windowSize + 1) 0) [0..fromIntegral $ n - 1]
  where
    foo (eps, weights) k = let xs = take windowSize $ drop k points
                            in _a
