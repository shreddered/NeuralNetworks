{-# LANGUAGE OverloadedStrings #-}
module Data.Geo
  ( Point(..)
  , FeatureItem(..)
  , Features(..)
  , clusterizeFeatures
  ) where

import Data.Aeson
import Data.Cluster

data Point = Point
  { lat :: Double
  , lng :: Double
  } deriving (Eq, Show)

instance FromJSON Point where
  parseJSON = withObject "Point" $ (pointFromList <$>) . (.: "coordinates")

instance ToJSON Point where
  toJSON (Point lat_ lng_) = object [ "coordinates" .= [lng_, lat_]
                                    , "type" .= ("Point" :: String) ]

instance ClusterData Point where
  -- good enough for demostration purposes
  mean lst = Point { lng = (sum $ map lng lst) / (fromIntegral $ length lst)
                   , lat = (sum $ map lat lst) / (fromIntegral $ length lst)
                   }
  distance (Point lng1 lat1) (Point lng2 lat2) = -- haversine formula
    let p1 = lat1 * pi / 180
        p2 = lat2 * pi / 180
        dp = p2 - p1
        dl = (lng2 - lng1) * pi / 180
        a = (sin $ dp / 2)^2 + (cos p1) * (cos p2) * (sin $ dl / 2)^2
        c = 2 * (atan2 (sqrt a) (sqrt $ 1 - a))
     in 6.376e+6 * c

pointFromList :: [Double] -> Point
pointFromList lst = Point (lst !! 1) (lst !! 0)

newtype FeatureItem = FeatureItem
  { geometry :: Point
  } deriving Show

instance FromJSON FeatureItem where
  parseJSON = withObject "FeatureItem" $ \obj -> FeatureItem
    <$> (obj .: "geometry")

instance ToJSON FeatureItem where
  toJSON (FeatureItem p) = object [ "geometry" .= p
                                  , "type" .= ("Feature" :: String)
                                  , "properties" .= Null ]

newtype Features = Features
  { features :: [FeatureItem]
  } deriving Show

instance FromJSON Features where
  parseJSON = withObject "Features" $ \obj -> Features
    <$> (obj .: "features")

instance ToJSON Features where
  toJSON (Features lst) = object [ "features" .= lst
                                 , "type" .= ("FeatureCollection" :: String) ]

clusterizeFeatures :: Maybe Features -> [Point] -> (Features, [Features])
clusterizeFeatures Nothing _ = error "Error while parsing input data"
clusterizeFeatures (Just (Features lst)) centers =
    (makeFeatures centers', makeFeatures <$> clusters)
  where
    points = map geometry lst
    (clusters, centers') = kMeans points centers

makeFeatures points = Features lst
  where lst = map (FeatureItem $) points
