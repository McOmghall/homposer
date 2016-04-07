module Main where

import Euterpea

main :: IO ()
main = do
  play $ line $ map (uncurry transpose) $ zip [0..12] (repeat ((c 4 qn) :=: (e 4 qn) :=: (g 4 qn)))
