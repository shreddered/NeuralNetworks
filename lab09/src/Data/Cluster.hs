module Data.Cluster 
  ( ClusterData(..)
  , kMeans
  ) where

import Data.Function (on)
import Data.List (minimumBy, partition)
import Data.Map (Map)
import qualified Data.Map as Map

class Eq a => ClusterData a where
  distance :: a -> a -> Double
  mean :: [a] -> a

kMeans :: ClusterData a => [a] -> [a] -> ([[a]], [a])
kMeans points centers
  | newCenters == centers = (clusters, centers)
  | length centers /= length newCenters = error "WTF"
  | otherwise             = kMeans points newCenters
  where
    nearestCenter point = minimumBy (compare `on` (distance point)) centers
    clusters = groupBy2 (\p1 p2 -> nearestCenter p1 == nearestCenter p2) points
    newCenters = mean <$> clusters

-- utility function
groupBy2 :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy2 = go [] where
  go acc comp [] = acc
  go acc comp (h:t) =
    let (hs, nohs) = partition (comp h) t
    in go ((h:hs):acc) comp nohs
