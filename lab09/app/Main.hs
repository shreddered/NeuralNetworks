{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics

import Data.Aeson
import Data.Cluster
import Data.Geo
import Options.Applicative

data Args = Args String String

argsParser :: Parser Args
argsParser = Args
  <$> strOption
      ( long "input"
     <> short 'i'
     <> metavar "INPUT"
     <> help "Input file with data to be clusterized" )
  <*> strOption
      ( long "output"
     <> short 'o'
     <> metavar "OUTPUT"
     <> help "Output file" )

opts = info (argsParser <**> helper) (fullDesc <> progDesc "Clusterize data in INPUT"
                                               <> header "Clusterize data for lab09")

clusterize :: Args -> IO ()
clusterize (Args input output) = do
  features_ <- decodeFileStrict input
  let newFeatures = clusterizeFeatures features_
  encodeFile output newFeatures

main :: IO ()
main = do
  args <- execParser opts
  clusterize args
