module Main where

import Lib
import Data.List.Split
import Data.Tree (drawTree)

main :: IO ()
main = do
  contents <- readFile "input.txt"
  let orbitPairs = map (\[x, y] -> (x, y)) (map (splitOn ")") (lines contents))
  let orbitTree = orbits orbitPairs
  -- putStr $ drawTree orbitTree
  print $ orbitalTransfers orbitTree "YOU" "SAN"
