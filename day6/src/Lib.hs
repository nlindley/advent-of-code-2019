module Lib
    ( orbitCount
    ) where

import Data.Tree (Tree, flatten, unfoldTree, levels)

buildNode :: [(String, String)] -> String -> (String, [String])
buildNode pairs x = (x, map snd (filter ((== x) . fst) pairs))

orbitCount :: [(String, String)] -> Int
orbitCount pairs =
    let l = levels $ unfoldTree (buildNode pairs) "COM"
        withIndex = zip [0..(length l - 1)] l
    in foldr (\(x, y) total -> total + x * (length y)) 0 withIndex
