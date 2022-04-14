module Numeric.Neural.RBF
    ( RBFNetwork
    ) where

import           Control.Applicative
import           Data.Vector (Vector)
import qualified Data.Vector as V

-- data type for polymorphic activation function
data ActivationFunction a = ActivationFunction
    { primary :: a -> a
    , derivative :: a -> a
    }

-- RBF Network (very simple)
-- although it can be defined in category theory fashion
data RBFNetwork a = RBFNetwork
    { activationFunc :: ActivationFunction a
    , centers :: [Vector a]
    , weights :: Vector a
    , learningRate :: a
    }

-- create empty RBF
emptyRBF :: (Floating a)
         => a
         -> ActivationFunction a
         -> [Vector a]
         -> RBFNetwork a
emptyRBF lr func cntrs = RBFNetwork
    { activationFunc = func
    , centers = cntrs
    , weights = V.empty
    , learningRate = lr
    }

trainRBF :: (Floating a, Traversable t)
         => RBFNetwork a        -- Network to train
         -> t (Vector a, a)     -- traversable of (vector, output)
         -> t (Vector a, a)     -- traversable of (vector, target output)
         -> (RBFNetwork a, [a]) -- (network, [errors])
trainRBF (RBFNetwork func ctrs _ lr) input target = (network, errors)
  where
    network = RBFNetwork func ctrs ws lr
    phis = map createPhi ctrs
    createPhi c = \x -> exp (V.sum $ V.zipWith (\xi cji -> (xi - cji)^2) x c)
    (ws, errors) = _a
