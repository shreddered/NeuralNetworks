module Data.Neuron
    ( ActivationFunction (..)
    , train
    ) where

-- for now I will keep it simple
data ActivationFunction = ActivationFunction
    { primary    :: Double -> Double
    , derivative :: Double -> Double
    , out        :: Double -> Double
    }

-- train function
train :: [Double]           -- reference
      -> Double             -- learning rate
      -> ActivationFunction -- activation function
      -> ([Double], [Int])  -- (final weights, list of errors)
train = trainHelper (replicate 5 0, []) -- a dirty hack

-- train helper (epoch loop)
-- tail recursion power
trainHelper :: ([Double], [Int])  -- (weights, error)
            -> [Double]           -- reference (boolean function)
            -> Double             -- learning rate
            -> ActivationFunction -- activation function
            -> ([Double], [Int])  -- final weights
trainHelper (weights, err@(0:_)) _ _ _ = (weights, reverse err)
trainHelper (weights, err) func lr af = trainHelper (weights', err' : err) func lr af
  where
    (weights', output) = epoch weights func lr af
    -- Hamming distance
    diff = zipWith (/=) func output
    err' = (sum . map fromEnum) diff

-- one epoch
epoch :: [Double]             -- old weights
      -> [Double]             -- reference function
      -> Double               -- learning rate
      -> ActivationFunction   -- activation function
      -> ([Double], [Double]) -- (weights, output)
epoch weights func lr af = (weights', output)
  where
    indexes = [ [1, a, b, c, d] | a <- [0, 1]
                                , b <- [0, 1]
                                , c <- [0, 1]
                                , d <- [0, 1]
              ]
    -- the epoch loop itself (nothing but a fancy fold)
    (weights', output') = foldl (deltaRule lr af) (weights, []) (zip indexes func)
    output = reverse output'

-- auxiliary function for delta-rule folding
deltaRule :: Double               -- learning rate
          -> ActivationFunction   -- activation function
          -> ([Double], [Double]) -- (old weights, neuron output)
          -> ([Double], Double)   -- (binary vector, function value)
          -> ([Double], [Double]) -- (new weights, neuron output)
deltaRule learningRate activationFunc (weights, output) (vec, t) = (weights', output')
  where
    f = primary activationFunc
    f' = derivative activationFunc
    net = sum (zipWith (*) weights vec)
    y = (out activationFunc) (f net)
    newWeight w x = w + learningRate * (t - y) * (f' net) * x
    weights' = zipWith newWeight weights vec
    output' = y : output
