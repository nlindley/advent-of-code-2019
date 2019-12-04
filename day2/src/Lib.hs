module Lib
    ( someFunc
    ) where

import Data.Vector

applyOp :: (Int -> Int -> Int) -> Int -> Vector Int -> Vector Int
applyOp f i v = let ix = v ! (i + 1)
                    iy = v ! (i + 2)
                    iz = v ! (i + 3)
                    x = v ! ix
                    y = v ! iy
                in v // [(iz, f x y)]

run :: Int -> Vector Int -> Vector Int
run pc v
    | opcode == 99 = v
    | opcode == 1  = run (pc + 4) $ applyOp (+) pc v
    | opcode == 2  = run (pc + 4) $ applyOp (*) pc v
    where opcode = v ! pc

someFunc :: [Int] -> [Int]
someFunc xs = toList $ run 0 (fromList xs)
