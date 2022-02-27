module Main where

data ActivationFunction = ActivationFunction { f :: Double -> Bool
                                             , f' :: Double -> Double
                                             }


main :: IO ()
main = putStrLn "Hello, Haskell!"
