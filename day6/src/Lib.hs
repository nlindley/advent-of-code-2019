module Lib
    ( orbitCount
    , orbits
    , orbitalTransfers
    ) where

import Data.Tree (Tree( Node ), Forest, flatten, foldTree, levels, unfoldTree)
import Data.Set (Set, fromList, union, intersection, difference)

buildNode :: [(String, String)] -> String -> (String, [String])
buildNode pairs x = (x, map snd (filter ((== x) . fst) pairs))

orbits :: [(String, String)] -> Tree String
orbits pairs = unfoldTree (buildNode pairs) "COM"

orbitCount :: [(String, String)] -> Int
orbitCount pairs =
    let l = levels $ orbits pairs
        withIndex = zip [0..(length l - 1)] l
    in foldr (\(x, y) total -> total + x * (length y)) 0 withIndex

pathLength :: [String] -> [String] -> Int
pathLength x y =
    let xSet = fromList x
        ySet = fromList y
        combined = union xSet ySet
        common = intersection xSet ySet
    in length $ difference combined common

orbitalTransfers :: Tree String -> String -> String -> (Maybe Int)
orbitalTransfers orbitTree from to =
    let fromPath = findNeedle [] from orbitTree
        toPath   = findNeedle [] to orbitTree
    in case (fromPath, toPath) of
        (Just x, Just y) -> Just(pathLength x y)
        otherwise        -> Nothing

findNeedleInTrees :: [String] -> String -> Forest String -> Maybe [String]
findNeedleInTrees acc needle [] = Nothing
findNeedleInTrees acc needle (tree:trees) =
    case findNeedle acc needle tree of
        Nothing -> findNeedleInTrees acc needle trees
        Just path -> Just path

findNeedle :: [String] -> String -> Tree String -> Maybe [String]
findNeedle acc needle (Node rootLabel [])
    | needle == rootLabel = Just acc
    | otherwise           = Nothing
findNeedle acc needle (Node rootLabel subtrees)
    | needle == rootLabel = Just acc
    | otherwise           = findNeedleInTrees (rootLabel:acc) needle subtrees
