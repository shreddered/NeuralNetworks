module Main where

import Control.Arrow

import Data.Neuron
import Data.List (intercalate, unlines)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Text.Printf

prettyPrint :: [([Double], [Double])] -> [Int] -> [String]
prettyPrint = zipWith (\(weights, output) err -> (intercalate "," $ printf "% .4f" <$> weights) ++ "\t" ++ (intercalate "," $ printf "%.f" <$> output) ++ "\t" ++ (printf "%2d" err))

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
      table1 = train 0.3 thresholdAF func
      errors = map (sum . map fromEnum . zipWith (/=) func . snd) table1
      table2 = train 0.3 logisticAF func
      errors' = map (sum . map fromEnum . zipWith (/=) func . snd) table2
      thresholdPlot = zip ([1..] :: [Int]) errors
      logisticPlot = zip ([1..] :: [Int]) errors'
  (putStrLn . unlines) $ prettyPrint table1 errors
  (putStr . unlines) $ prettyPrint table2 errors'
  toFile def "images/plot1.png" $ do
    layout_title .= "Пороговая функция активации"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Количество ошибок"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [thresholdPlot])
    plot (points "Количество ошибок за эпоху" (thresholdPlot))
  toFile def "images/plot2.png" $ do
    layout_title .= "Логистическая функция активации"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Количество ошибок"
    setColors [opaque blue, opaque red]
    plot (line "Количество ошибок за эпоху" [logisticPlot])
    plot (points "Количество ошибок за эпоху" (logisticPlot))
