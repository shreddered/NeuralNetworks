module Data.Extrapolation
    ( generatePoints
    , train
    , getPredictions
    ) where

import Control.Arrow

-- function for point generation
generatePoints :: (Double, Double) -- (a, b)
               -> Int              -- number of points
               -> [Double]         -- points
generatePoints (a, b) n = let step = (b - a) / (realToFrac n)
                           in take n $ iterate (+ step) a

train :: Double               -- learning rate
      -> [Double]             -- points
      -> Int                  -- number of epochs
      -> Int                  -- size of window
      -> ([Double], [Double]) -- (epsilons, weights)
train learningRate points n windowSize =
    foldl something ([], replicate (windowSize + 1) 0) $ replicate n [0..18-windowSize]
  where
    something x = epoch x . map (\k -> (take windowSize $ drop k points, points !! (k + windowSize + 1)))
    epoch (es, weights) = first (: es) . first sqrt . foldl foo (0, weights)
    foo params@(_, weights) (column, x) = let net = sum (zipWith (*) column (tail weights)) + (head weights)
                                              delta = x - net
                                              newWeights = zipWith (\xn w -> w + learningRate * delta * xn) (1:column)
                                           in ((+ delta^2) *** newWeights) params

getPredictions :: [Double] -- points
               -> Int      -- size of window
               -> [Double] -- weights
               -> ([Double], [Double]) -- predictions
getPredictions points windowSize weights = first (reverse . take (20 - windowSize)) $ (iterate foo ([], initial)) !! 20
  where
    initial = drop (20 - windowSize) points
    (w0:weights') = weights
    foo (est, xs) = ((head xs):est, (tail xs) ++ [sum (zipWith (*) weights' xs) + w0])
