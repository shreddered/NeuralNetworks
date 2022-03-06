module Main where

import Data.Neuron

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

main :: IO ()
main = do
  let thresholdAF = ActivationFunction
          { primary  = \x -> if x < 0 then 0 else 1
          , derivative = const 1
          , out = id
          }
      logisticAF = ActivationFunction
          { primary = (/ 2) . (+ 1) . tanh
          , derivative = (/ 2) . (1-) . (^ 2) . tanh
          , out = \x -> if x < 0.5 then 0 else 1
          }
      func = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1 ]
      (weights, errors) = train func 0.3 thresholdAF
      thresholdPlot = zip ([1..] :: [Int]) errors
      (weights', errors') = train func 0.3 logisticAF
      logisticPlot = zip ([1..] :: [Int]) errors'
  print weights
  toFile def "images/plot1.png" $ do
    layout_title .= "Пороговая функция активации"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Количество ошибок"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [thresholdPlot])
    plot (points "Количество ошибок за эпоху" (thresholdPlot))
  print weights'
  toFile def "images/plot2.png" $ do
    layout_title .= "Логистическая функция активации"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Количество ошибок"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [logisticPlot])
    plot (points "Количество ошибок за эпоху" (logisticPlot))
