module Lib
    ( run
    ) where

import Data.Vector

if' :: Bool -> a -> a -> a
if' True  x _ = x
if' False _ y = y

input modes pc v i =
    let pos = location v 1 modes pc
    in v // [(pos, i)]

output modes i v =
    let pos = location v 1 modes i
    in v ! pos

location v n modes pc
    | mode == 1 = pc + n
    | otherwise = v ! (pc + n)
    where mode = modes `div` (10 ^ (n - 1)) `rem` 10

applyOp :: (Int -> Int -> Int) -> Int -> Int -> Vector Int -> Vector Int
applyOp f modes i v =
    let ix = location v 1 modes i
        iy = location v 2 modes i
        iz = v ! (i + 3)
        x = v ! ix
        y = v ! iy
    in v // [(iz, f x y)]

jumpIfTrue modes pc v =
    let p1 = location v 1 modes pc
        p2 = location v 2 modes pc
    in if' ((v ! p1) /= 0) (v ! p2) (pc + 3)

jumpIfFalse modes pc v =
    let p1 = location v 1 modes pc
        p2 = location v 2 modes pc
    in if' ((v ! p1) == 0) (v ! p2) (pc + 3)

lessThan modes pc v =
    let p1 = location v 1 modes pc
        p2 = location v 2 modes pc
        out = v ! (pc + 3)
    in if' ((v ! p1) < (v ! p2)) (v // [(out, 1)]) (v // [(out, 0)])

equals modes pc v =
    let p1 = location v 1 modes pc
        p2 = location v 2 modes pc
        out = v ! (pc + 3)
    in if' ((v ! p1) == (v ! p2)) (v // [(out, 1)]) (v // [(out, 0)])

run :: Int -> Vector Int -> Int -> [Int] -> [Int]
run pc v i o
    | opcode == 99 = o
    | opcode ==  1 = run (pc + 4) (applyOp (+) modes pc v) i o
    | opcode ==  2 = run (pc + 4) (applyOp (*) modes pc v) i o
    | opcode ==  3 = run (pc + 2) (input modes pc v i) i o
    | opcode ==  4 =
        let out = output modes pc v
        in run (pc + 2) v i (o Prelude.++ [out])
    | opcode ==  5 = run (jumpIfTrue modes pc v) v i o
    | opcode ==  6 = run (jumpIfFalse modes pc v) v i o
    | opcode ==  7 = run (pc + 4) (lessThan modes pc v) i o
    | opcode ==  8 = run (pc + 4) (equals modes pc v) i o
    where
        opcode = (v ! pc) `rem` 100
        modes = (v ! pc) `div` 100
