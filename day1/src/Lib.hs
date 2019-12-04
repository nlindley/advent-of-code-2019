module Lib
    ( totalFuelRequired
    ) where

fuelNeeded :: Int -> Int
fuelNeeded mass = (div mass 3) - 2

totalFuelRequired :: [String] -> Int
totalFuelRequired masses = foldr (+) 0 $ map (fuelNeeded . read) masses
