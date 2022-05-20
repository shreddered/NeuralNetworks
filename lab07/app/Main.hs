module Main where

import Numeric.Neural.Hopfield
import Numeric.LinearAlgebra

main :: IO ()
main = do
  let six   = [  1,  1,  1,  1
              ,  1, -1, -1, -1
              ,  1, -1, -1, -1
              ,  1,  1,  1,  1
              ,  1, -1, -1,  1
              ,  1, -1, -1,  1
              ,  1,  1,  1,  1
              ]
      seven = [  1,  1,  1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              ]
      four  = [  1, -1, -1,  1
              ,  1, -1, -1,  1
              ,  1, -1, -1,  1
              ,  1,  1,  1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              , -1, -1, -1,  1
              ]
      vectors = [six, seven, four]
      network = initHopfield vectors
  putStrLn "Weights after initialization:"
  print $ weights network
  putStrLn "Check original images:"
  putStrLn "6:"
  print $ matrix 4 $ evalHopfield network six
  putStrLn "4:"
  print $ matrix 4 $ evalHopfield network four
  putStrLn "7:"
  print $ matrix 4 $ evalHopfield network seven
  let six'   = [  1,  1,  1, -1
               ,  1, -1,  1, -1
               ,  1, -1, -1, -1
               ,  1,  1,  1,  1
               ,  1, -1, -1,  1
               ,  1, -1, -1,  1
               ,  1,  1,  1,  1
               ]
      seven' = [  1,  1,  1,  1
               ,  1, -1, -1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               ]
      four'  = [  1, -1, -1,  1
               ,  1, -1, -1,  1
               ,  1,  1, -1,  1
               ,  1, -1,  1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               , -1, -1, -1,  1
               ]
      six'' = evalHopfield network six'
      seven'' = evalHopfield network seven'
      four'' = evalHopfield network four'
  putStrLn "After corruption:"
  putStrLn "6:"
  print $ matrix 4 six''
  putStrLn "7:"
  print $ matrix 4 seven''
  putStrLn "4:"
  print $ matrix 4 four''
