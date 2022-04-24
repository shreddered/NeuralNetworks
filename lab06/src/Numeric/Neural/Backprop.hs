module Numeric.Neural.Backprop
    ( BackpropNetwork(..)
    , ActivationFunc(..)
    , trainBackprop
    , emptyBackprop
    ) where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))
import System.Random

data ActivationFunc = ActivationFunc (Double -> Double) (Double -> Double)

data BackpropNetwork = BackpropNetwork
    { architecture :: (Int, Int, Int)
    , hiddenWeights :: Matrix Double
    , outWeights :: Matrix Double
    , activationFunction :: ActivationFunc
    , learningRate :: Double
    , epsilon :: Double
    }

emptyBackprop :: RandomGen g
              => g
              -> (Int, Int, Int)
              -> ActivationFunc
              -> Double
              -> Double
              -> BackpropNetwork
emptyBackprop gen arch@(n, j, m) func lr eps = BackpropNetwork arch hw ow func lr eps
  where
    hw = (j >< (n + 1)) $ randomRs (-1, 1) gen
    ow = (m >< j) $ randomRs (-1, 1) gen

trainBackprop :: BackpropNetwork                 -- network to train
              -> [Double]                        -- input vector
              -> [Double]                        -- target output
              -> (BackpropNetwork, [[Double]]) -- (net, output at each epoch)
trainBackprop (BackpropNetwork (n, j, m) hw ow func@(ActivationFunc f f') lr e) input target =
    (BackpropNetwork (n, j, m) hiddenWeights' outWeights' func lr e, reverse outputs)
  where
    vec = ((n + 1) >< 1) input
    t = (m >< 1) target
    (hiddenWeights', outWeights', outputs) = (head . dropWhile bigError . tail) table 
    bigError (_, _, (y:_)) = let err = sqrt $ sum $ zipWith (\x y -> (x - y)^2) target y
                              in err > e
    table = iterate trainHelper (hw, ow, [])
    trainHelper (hw_, ow_, tbl) = let net1 = hw_ <> vec
                                      out1 = f `cmap` net1
                                      net2 = ow_ <> out1
                                      y = f `cmap` net2
                                      derivative2 = f' `cmap` net2
                                      delta2 = zipMatrix (*) derivative2 $ t - y
                                      derivative1 = f' `cmap` net1
                                      delta1 = zipMatrix (*) derivative1 $ (tr ow) <> delta2
                                      hw' = hw_ + scale lr (delta1 <> (tr vec))
                                      ow' = ow_ + scale lr (delta2 <> (tr out1))
                                   in (hw', ow', (toList $ flatten y) : tbl)

zipMatrix :: (Double -> Double -> Double)
          -> Matrix Double
          -> Matrix Double
          -> Matrix Double
zipMatrix f mat1 mat2 = (n >< m) $ zipWith f lst1 lst2
  where
    lst1 = toList $ flatten mat1
    lst2 = toList $ flatten mat2
    n = rows mat1
    m = cols mat1
