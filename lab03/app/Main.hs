module Main where

import Data.Extrapolation

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

-- my function
func t = sqrt (0.1 * t) + 1

-- generate plot
signal :: [Double] -> [(Double, Double)]
signal ts = [ (t, func t) | t <- ts ]

main :: IO ()
main = toFile def "images/plot1.png" $ do
  layout_title .= "График исходной функции"
  layout_x_axis . laxis_title .= "t"
  layout_y_axis . laxis_title .= "x"
  plot (line "x(t) = sqrt(0.1 * t) + 1" [ signal [1,1.01..5] ])
