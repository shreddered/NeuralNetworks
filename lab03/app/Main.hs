module Main where

import Data.Extrapolation
import Data.List (intercalate)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Text.Printf

-- my function
func t = sqrt (0.1 * t) + 1

generatePoints' :: (Double, Double) -- (a, b)
                -> Int              -- number of points
                -> [Double]         -- points
generatePoints' (a, b) n = let step = (b - a) / (realToFrac n)
                            in reverse (take n (iterate (\x -> x - step) b))

-- generate plot
signal :: [Double] -> [(Double, Double)]
signal ts = [ (t, func t) | t <- ts ]

main :: IO ()
main = do
  let ts = generatePoints' (5, 9) 20
      points = func <$> generatePoints (1, 5) 20
      (eps, weights) = train 0.1 points 4000 5
      (x1, x2) = getPredictions points 5 weights
  putStrLn $ intercalate ", " (printf "%.4f" <$> weights)
  putStrLn $ printf "%.4f" (head eps)
  toFile def "images/plot5.png" $ do
    layout_title .= "График исходной функции на интервале [a, 2b-a]"
    layout_x_axis . laxis_title .= "t"
    layout_y_axis . laxis_title .= "x"
    setColors [opaque blue, opaque red]
    plot (line "x(t) = sqrt(0.1 * t) + 1" [ signal [1,1.01..9] ])
    plot (line "Аппроксимация" [zip ts (x1 ++ x2)])
