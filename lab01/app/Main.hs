module Main where

import Control.Arrow
import Control.Monad (mapM_)

import Data.Neuron
import Data.List (intercalate, unlines)

import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo

import Text.Printf

prettyPrint :: [([Double], [Int])] -> [Int] -> [String]
prettyPrint = zipWith (\(weights, output) err -> (intercalate "," $ printf "% .4f" <$> weights)
    ++ "\t" ++ (intercalate "," $ printf "%1d" <$> output) ++ "\t" ++ (printf "%2d" err))

main :: IO ()
main = do
  let thresholdAF = ActivationFunction
          { primary  = \x -> if x < 0 then 0 else 1
          , derivative = const 1
          }
      logisticAF = ActivationFunction
          { primary = (/ 2) . (+ 1) . tanh
          , derivative = (/ 2) . (1-) . (^ 2) . tanh
          }
      func = [ 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 1 ]
      vectors = [ [1, a, b, c, d] | a <- [0, 1]
                                  , b <- [0, 1]
                                  , c <- [0, 1]
                                  , d <- [0, 1]
                ]
      table1 = train 0.3 thresholdAF $ zip vectors func
      errors = map (sum . map fromEnum . zipWith (/=) func . snd) table1
      table2 = train 0.3 logisticAF $ zip vectors func
      errors' = map (sum . map fromEnum . zipWith (/=) func . snd) table2
      thresholdPlot = zip ([1..] :: [Int]) errors
      logisticPlot = zip ([1..] :: [Int]) errors'
      (weights, vectors', n) = train' 0.3 logisticAF (zip vectors func)
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
  putStrLn (intercalate "," $ printf "% .4f" <$> weights)
  mapM_ (putStrLn . intercalate "," . (<$>) (printf "%.f") ) vectors'
  print n
