module Lib
    ( minimumDistance
    ) where

import Data.List
import Data.List.Split

instructions wires = map (splitOn ",") wires

parseInstruction inst
    | dir == 'R' = replicate distance (1, 0)
    | dir == 'L' = replicate distance (-1, 0)
    | dir == 'U' = replicate distance (0, 1)
    | dir == 'D' = replicate distance (0, -1)
    where (dir, distance) = (head inst, read $ tail inst)

movements wire = wire >>= parseInstruction

addPairs a b = (fst a + fst b, snd a + snd b)
points = scanl addPairs (0, 0)

manhattanDistance x = (abs $ fst x) + (abs $ snd x)

minimumDistance :: String -> Int
minimumDistance input =
    let insts = instructions $ lines input
        wire1 = movements $ head insts
        wire2 = movements $ last insts
        points1 = points wire1
        points2 = points wire2
        intersections = intersect points1 points2
    in minimum $ filter (>0) $ map manhattanDistance intersections
