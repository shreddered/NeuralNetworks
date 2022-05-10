module Data.Cluster 
  ( ClusterData
  , kMeans
  ) where

import Data.Function (on)
import Data.List (groupBy, minimumBy)

class Eq a => ClusterData a where
  distance :: a -> a -> Double
  mean :: [a] -> a

kMeans :: ClusterData a => [a] -> [a] -> [[a]]
kMeans points centers
  | newCenters == centers = clusters
  | otherwise             = kMeans points newCenters
  where
    nearestCenter point = minimumBy (compare `on` (distance point)) centers
    clusters = groupBy (\p1 p2 -> nearestCenter p1 == nearestCenter p2) points
    newCenters = mean <$> clusters
