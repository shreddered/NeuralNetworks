module Data.Geo
  ( Geometry(..)
  , FeatureItem(..)
  , Features(..)
  ) where

import Data.Aeson

data Geometry = Geometry
  { coordinates :: [Double]
  , geometryType :: String
  } deriving Show

instance FromJSON Geometry where
  parseJSON = withObject "Geometry" $ \obj -> Geometry
    <$> (obj .: "coordinates") <*> (obj .: "type")

data FeatureItem = FeatureItem
  { geometry :: Geometry
  , properties :: Value
  , featureType :: String
  } deriving Show

instance FromJSON FeatureItem where
  parseJSON = withObject "FeatureItem" $ \obj -> FeatureItem
    <$> (obj .: "geometry") <*> (obj .: "properties") <*> (obj .: "type")

data Features = Features
  { features :: [FeatureItem]
  , featuresType :: String
  } deriving Show

instance FromJSON Features where
  parseJSON = withObject "Features" $ \obj -> Features
    <$> (obj .: "features") <*> (obj .: "type")

clusterizeFeatures :: Maybe Features -> Features
clusterizeFeatures Nothing = error "Error while parsing input data"
clusterizeFeatures (Just (Features lst t)) = Features lst t
