module Data.Neuron
    ( ActivationFunction (..)
    , train
    , combinations
    ) where

import Control.Arrow
import Data.List
    ( tails
    )

-- for now I will keep it simple
data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    , out        :: Double -> Double
    }

-- train function
train :: Double                 -- learning rate
      -> ActivationFunction     -- activation function
      -> [([Double], Double)]   -- original function, i.e. [(vector, value)]
      -> [([Double], [Double])] -- [(weights, output)]
train learningRate activationFunc func =
    tail $ takeWhileInclusive (snd . second pred) infiniteTable
  where
    pred :: [Double] -> Bool
    pred = or . zipWith (/=) (map snd func)
    infiniteTable = iterate foo (replicate 5 0, replicate 16 1)
    foo = epoch learningRate activationFunc func

-- one epoch
epoch :: Double               -- learning rate
      -> ActivationFunction   -- activation function
      -> [([Double], Double)] -- original function
      -> ([Double], [Double]) -- old weights
      -> ([Double], [Double]) -- (weights, output)
epoch learningRate activationFunc func (weights, _) =
   (second reverse . foldl widrowHoff (weights, [])) func
  where
    widrowHoff x@(weights, output) (vector, t) = (zipWith newWeight vector *** (:) y) x
      where
        f = primary activationFunc
        f' = derivative activationFunc
        net = sum $ zipWith (*) weights vector
        y = (out activationFunc) (f net)
        newWeight x w = w + learningRate * (t - y) * (f' net) * x

-- generate combinations
combinations :: Integral a => a -> [b] -> [[b]]
combinations 0 lst = [[]]
combinations n lst = do
  (x:xs) <- tails lst
  rest   <- combinations (n - 1) xs
  return $ x : rest

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ []        = []
takeWhileInclusive pred (x:xs) = x : if pred x then takeWhileInclusive pred xs
                                               else []
