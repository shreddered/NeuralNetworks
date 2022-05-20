module Main where

import Numeric.Neural.Hopfield

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
  let six'  = [  1,  1,  1, -1
              ,  1, -1,  1, -1
              ,  1, -1, -1, -1
              ,  1,  1,  1,  1
              ,  1, -1, -1,  1
              ,  1, -1, -1,  1
              ,  1,  1,  1,  1
              ]
  print $ evalHopfield network six'
