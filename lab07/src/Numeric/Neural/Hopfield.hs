{-# LANGUAGE MultiWayIf #-}
module Numeric.Neural.Hopfield 
    ( HopfieldNetwork(..)
    , initHopfield
    , evalHopfield
    ) where

import Numeric.LinearAlgebra
import Prelude hiding ((<>))

newtype HopfieldNetwork = HopfieldNetwork { weights :: Matrix Double }

initHopfield :: [[Double]] -> HopfieldNetwork
initHopfield vectors = HopfieldNetwork w'
  where
    vectors' = scanl1 checkLength vectors
    w = sum $ map makeMatrix vectors
    makeMatrix vec = (col vec) <> (row vec)
    -- zero out main diag
    n = rows w
    w' = accum w (\_ _ -> 0) [((i, i), 0) | i <- [0..n-1]]

evalHopfield :: HopfieldNetwork -> [Double] -> [Double]
evalHopfield network@(HopfieldNetwork w) vec
    | vec == out = out
    | otherwise  = evalHopfield network out
  where
    net' = (row vec) <> w
    net = toList $ flatten net'
    out = zipWith (\netK yOld -> if | netK < 0 -> -1
                                    | netK > 0 -> 1
                                    | otherwise -> yOld
                  ) net vec

checkLength vec1 vec2
  | length vec1 == length vec2 = vec2
  | otherwise                  = error "Images must have the same size"
