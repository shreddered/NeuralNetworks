module Data.Neuron
    ( ActivationFunction (..)
    , train
    ) where

-- for now I will keep it simple
data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    }

-- train function
train :: [Double]           -- reference
      -> Double             -- learning rate
      -> ActivationFunction -- activation function
      -> [Double]           -- final weights
train = trainHelper (replicate 4 0, 1) -- a dirty hack

-- train helper (epoch loop)
trainHelper :: ([Double], Double) -- (weights, error)
            -> [Double]           -- reference (boolean function)
            -> Double             -- learning rate
            -> ActivationFunction -- activation function
            -> [Double]           -- final weights
trainHelper (weights, 0) _ _ _ = weights
trainHelper (weights, _) func lr af = trainHelper (weights', err') func lr af
  where
    (weights', output) = epoch weights func lr af
    -- Hamming distance
    diff = zipWith ( (abs .) . (-) ) func output
    err' = sum diff

epoch :: [Double]
      -> [Double]
      -> Double
      -> ActivationFunction
      -> ([Double], [Double])
epoch weights func lr af = (weights', output)
  where
    indexes = [ [a, b, c, d] | a <- [0, 1]
                             , b <- [0, 1]
                             , c <- [0, 1]
                             , d <- [0, 1]
              ]
    (weights', output') = foldl (deltaRule lr af) (weights, []) (zip indexes func)
    output = reverse output'

deltaRule lr af (weights, output) (vec, value) = (weights', output')
  where
    f = primary af
    f' = derivative af
    curr = sum (zipWith ((*)) weights vec) + 1
    foo w x = w + lr * (value - (f curr)) * (f' curr) * x
    weights' = zipWith foo weights vec
    output' = curr : output
