module Main where

import Data.Neuron

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  let thresholdAF = ActivationFunction
          { primary  = \x -> if x < 0 then 0 else 1
          , derivative = const 1
          }
      singmoidalAF = ActivationFunction
          { primary = (/ 2) . (+ 1) . tanh
          , derivative = (/ 2) . (^ (-2)) . cosh
          }
      func = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1 ]
      (weights, errors) = train func 0.3 thresholdAF
      thresholdPlot = zip [1..] errors
  print weights
  toFile def "images/plot1.png" $ do
    layout_title .= "Пороговая функция активации"
    layout_x_axis . laxis_title .= "k"
    layout_y_axis . laxis_title .= "E(k)"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [thresholdPlot])
    plot (points "Количество ошибок за эпоху" (thresholdPlot))
