module Numeric.Neural.RBF
    ( ActivationFunction(..)
    , RBFNetwork(..)
    , emptyRBF
    , trainRBF
    ) where

import Control.Applicative ((<*>), pure)
import Control.Arrow (second)

-- data type for polymorphic activation function
data ActivationFunction a = ActivationFunction (a -> a) (a -> a)

-- RBF Network (very simple)
-- although it can be defined in category theory fashion
data RBFNetwork a = RBFNetwork
    { activationFunc :: ActivationFunction a
    , centers :: [[a]]
    , weights :: [a]
    , learningRate :: a
    }

-- create empty RBF
emptyRBF :: (Floating a)
         => a                    -- learning rate
         -> ActivationFunction a -- activation function
         -> [[a]]    -- centers
         -> RBFNetwork a
emptyRBF lr func cntrs = RBFNetwork func cntrs [] lr

trainRBF :: (Floating a, Eq a)
         => RBFNetwork a          -- Network to train
         -> [([a], a)]            -- traversable of (vector, output)
         -> [([a], a)]            -- traversable of (vector, target output)
         -> (RBFNetwork a, [Int]) -- (network, [errors])
trainRBF (RBFNetwork func@(ActivationFunction f _) ctrs _ lr) input target = (second reverse) (network, errors)
  where
    network = RBFNetwork func ctrs ws lr
    j = length ctrs
    phis = map phi ctrs
    phi c = \x -> exp (sum $ zipWith (\xi cji -> (xi - cji)^2) x c)
    (ws, errors) = (head . dropWhile ((/= 0) . head . snd) . tail) table -- drop while err != 0
    table = iterate trainHelper (replicate (j + 1) 0, [])
    trainHelper (w, err) = let w' = epoch w
                            in (w', (hammingDistance w') : err)
    epoch w = foldl newWeights w input
    output w = f . sum . (zipWith (*) w) . (1:) . (phis <*>) . pure
    newWeights w (vec, t) = let inputs = 1 : (phis <*> pure vec)
                                y = output w vec
                             in zipWith (\wi phi_i -> wi + lr * (t - y) * phi_i) w inputs
    hammingDistance w = foldl (\err (vec, t) -> err + fromEnum (t /= (output w vec))) 0 target
