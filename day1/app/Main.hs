module Main where

import Lib

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ totalFuelRequired (lines contents)
