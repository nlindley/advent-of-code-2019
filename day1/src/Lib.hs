module Lib
    ( totalFuelRequired
    ) where

fuelNeeded :: Int -> Int
fuelNeeded mass
    | fuel <= 0 = 0
    | otherwise = fuel + (fuelNeeded fuel)
    where fuel = (div mass 3) - 2

totalFuelRequired :: [String] -> Int
totalFuelRequired masses = foldr (+) 0 $ map (fuelNeeded . read) masses
