module Data.Neuron
    ( ActivationFunction (..)
    , train
    , train'
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

trainWithDepth :: Int                  -- depth
               -> Double               -- learning rate
               -> ActivationFunction   -- activation function
               -> [([Double], Double)] -- original function, i.e. [(vector, value)]
               -> ([Double], Int)      -- (weights, number of epochs)
trainWithDepth n learningRate activationFunc func = foldl choose ([], 0) $ take n (tail infiniteTable)
  where
    choose ([], n) (weights, output) = if hasErrors output
                                          then ([], n + 1)
                                          else (weights, n + 1)
    choose x _                       = x
    hasErrors output = or (zipWith (/=) output $ map snd func)
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

train' :: Double                      -- learning rate
       -> ActivationFunction          -- activation function
       -> [([Double], Double)]        -- original function, i.e. [(vector, value)]
       -> ([Double], [[Double]], Int) -- (weights, vectors, number of epochs)
train' learningRate activationFunc func = head $ reverse (takeWhile (\(x, _, _) -> not $ null x) something)
  where
    len = length func
    something = map chooseFromSubsets $ reverse [1..len-1]
    trainProxy = trainWithDepth 120 learningRate activationFunc
    subsets n = combinations n func
    chooseFromSubsets n = foldl choose ([], [[]], 0) (subsets n)
    choose x subset = let (weights, n) = trainProxy subset
                          vectors = fst $ unzip subset
                       in if check weights
                             then (weights, vectors, n)
                             else x
    check [] = False
    check weights = foldr (foo weights) True func
    foo _ _ False = False
    foo weights (vector, t) True = let net = sum $ zipWith (*) weights vector
                                       y = (out activationFunc) (f net)
                                    in y == t
    f = primary activationFunc
