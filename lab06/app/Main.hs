module Main where

import Control.Monad (mapM_)
import Data.List (intercalate)
import Graphics.Rendering.Chart.Easy
import Graphics.Rendering.Chart.Backend.Cairo
import Numeric.Neural.Backprop
import System.Random
import Text.Printf

prettyPrintRow :: (Int, [Double], Double) -> String
prettyPrintRow (n, output, err) = (printf "|%4d|" n) ++ " ("
  ++ (intercalate ", " (printf "% 1.3f" <$> output)) ++ ") " ++ (printf "|%1.4f|" err)

prettyPrint :: [(Int, [Double], Double)] -> IO ()
prettyPrint = mapM_ (putStrLn . prettyPrintRow)

f t = (1 - exp(-t)) / (1 + exp(-t))
f' t = (1 - (f t)^2) / 2

main :: IO ()
main = do
  g <- newStdGen
  let activationFunc = ActivationFunc f f'
      emptyNet = emptyBackprop g (2, 1, 2) activationFunc 0.6 0.01
      (trainedNet, outputs) = trainBackprop emptyNet [1, 1, -1] [0.2, -0.1]
      errs = map (sqrt . sum . (zipWith (\x y -> (x - y)^2) [0.2, -0.1])) outputs
      table = zip3 [1..] outputs errs
      errPlot :: [(Double, Double)]
      errPlot = zip [1..] errs
  putStrLn $ printf "It took %d epochs to train a net" (length outputs)
  prettyPrint table
  toFile def "images/plot.png" $ do
    layout_title .= "Зависимость ошибки от номера эпохи"
    layout_x_axis . laxis_title .= "Номер эпохи"
    layout_y_axis . laxis_title .= "Среднеквадратичная ошибка"
    layout_x_axis . laxis_generate .= scaledAxis def (1.0, 150.0)
    layout_legend .= Nothing
    setColors [opaque blue, opaque red]
    plot (line "Зависимость ошибки от номера эпохи" [errPlot])
