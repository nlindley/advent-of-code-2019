module Main where

import Lib
import Data.List.Split

main :: IO ()
main = do
  contents <- readFile "input.txt"
  print $ head $ someFunc (map read $ splitOn "," contents :: [Int])
