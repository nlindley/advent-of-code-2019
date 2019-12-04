module Lib
    ( someFunc
    ) where

digits :: Int -> [Int]
digits = map (read . (:[])) . show

hasDouble [] = False
hasDouble [x] = False
hasDouble (x:y:xs)
    | x == y    = True
    | otherwise = hasDouble (y:xs)

neverDecreases [] = True
neverDecreases [x] = True
neverDecreases (x:y:xs)
    | y < x     = False
    | otherwise = neverDecreases (y:xs)

findPossibleCombos :: [Int]
findPossibleCombos = [x | x <- [125730..579381], hasDouble $ digits x, neverDecreases $ digits x]

someFunc :: IO ()
someFunc = print $ length findPossibleCombos
