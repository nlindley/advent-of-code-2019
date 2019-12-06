module Main where

import Lib
import Data.List.Split
import Data.Vector

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let output = run 0 (fromList (Prelude.map read $ splitOn "," contents)) 5 []
  print $ output
