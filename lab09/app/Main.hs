{-# LANGUAGE OverloadedStrings #-}

module Main where

import GHC.Generics

import Data.Aeson
import Data.Cluster
import Data.Geo
import Options.Applicative

data Args = Args String String String

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
     <> help "Output file for the centers" )
  <*> strOption
      ( long "clusters"
     <> short 'c'
     <> metavar "DATA_OUTPUT"
     <> help "Output file for clusters" )

opts = info (argsParser <**> helper) (fullDesc <> progDesc "Clusterize data"
                                               <> header "Clusterize data for lab09")

clusterize :: Args -> IO ()
clusterize (Args input output dat) = do
  features_ <- decodeFileStrict input
  let centers = [ Point 55.600829704418764 37.54316567449531 -- ЮЗАО
                , Point 55.65186771097298 37.63828869886312 -- ЮАО
                , Point 55.69834218575577 37.79226926034776 -- ЮВАО
                , Point 55.78873474483153 37.77088432455684 -- ВАО
                , Point 55.87886416743801 37.619595366286205 -- СВАО
                , Point 55.75279358679302 37.62078875615169 -- ЦАО
                , Point 55.844473214492446 37.52351325981977 -- САО
                , Point 55.828137329170794 37.42975615615748 -- СЗАО
                , Point 55.71854424704131 37.457765357183455 -- ЗАО
                , Point 55.99401975920554 37.19301455056182 -- ЗелАО
                , Point 55.36592619325288 37.14529856678398 -- Троицкий АО
                , Point 55.56516082259696 37.365400273220715 -- Новомосковский АО
                ]
      (newFeatures, clusters) = clusterizeFeatures features_ centers
  encodeFile output newFeatures
  encodeFile dat clusters

main :: IO ()
main = do
  args <- execParser opts
  clusterize args
