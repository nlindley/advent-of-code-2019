module Lib
    ( someFunc
    ) where

import Data.Map as Map

digits :: Int -> [Int]
digits = Prelude.map (read . (:[])) . show

hasDouble xs = (not . Map.null) $ Map.filter (== 2) $ fromAscListWith (+) (Prelude.map (\x -> (x, 1)) xs)

neverDecreases [] = True
neverDecreases [x] = True
neverDecreases (x:y:xs)
    | y < x     = False
    | otherwise = neverDecreases (y:xs)

findPossibleCombos :: [Int]
findPossibleCombos = [x | x <- [125730..579381], hasDouble $ digits x, neverDecreases $ digits x]

someFunc :: IO ()
someFunc = print $ length findPossibleCombos
