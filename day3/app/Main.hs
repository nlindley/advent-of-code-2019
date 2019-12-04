module Main where

import Lib

main :: IO ()
main = do
  content <- readFile "input.txt"
  print $ minimumDistance content
