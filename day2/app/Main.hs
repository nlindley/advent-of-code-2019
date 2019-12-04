module Main where

import Lib
import Data.List.Split

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let (noun, verb) = findInputs (map read $ splitOn "," contents :: [Int])
  print $ 100 * noun + verb
