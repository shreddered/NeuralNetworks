module Main where

import Data.List (intercalate)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Numeric.Neural.RBF
import Text.Printf (printf)

threshold x
  | x < 0     = 0
  | otherwise = 1

thresholdAF = ActivationFunction threshold id

prettyPrint vec = "(" ++ (intercalate ", " $ printf "%1.f" <$> vec) ++ ")"

main :: IO ()
main = do
  let func = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1 ] :: [Double]
      vectors = [ [a, b, c, d] | a <- [0, 1]
                               , b <- [0, 1]
                               , c <- [0, 1]
                               , d <- [0, 1]
                ]
      target = zip vectors func
      centers = map fst $ filter ((== 0) . snd) target
      input = [ ([ 0, 1, 1, 1 ], 1)
              , ([ 1, 0, 0, 0 ], 1)
              , ([ 1, 1, 0, 1 ], 0)
              , ([ 1, 1, 1, 0 ], 0)
              , ([ 0, 1, 0, 1 ], 1)
              ]
      trainVectors = fst $ unzip input
      network = emptyRBF 0.3 thresholdAF centers
      (trainedNetwork, errors) = trainRBF network input target
      errPlot = zip ([1..] :: [Int]) errors
  putStr "Using the following vectors: "
  putStrLn (intercalate ", " $ prettyPrint <$> trainVectors)
  putStrLn $ printf "It took %d epochs to train a net." (length errors)
  -- TODO: plot of errors
  toFile def "images/plot.png" $ do
    layout_title .= "Количество ошибок в каждой эпохе"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Количество ошибок"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [errPlot])
    plot (points "Количество ошибок за эпоху" (errPlot))
