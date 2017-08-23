module Main where

import Data.List as L
import Data.Array as A
import Data.Map as M
import Text.CSV
import Data.Graph.Inductive.Graph

import Lib
import BlockColor
import FloorNodes
import StartPoint
import EndPoint

isBlockConstraint :: [Int] -> Bool
isBlockConstraint (r:g:b:y:_) = (node_color_map M.! r /= Red) && (node_color_map M.! g /= Green) && (node_color_map M.! b /= Blue) && (node_color_map M.! y /= Yellow)
isBlockConstraint xs = False

blockList :: [[Int]]
blockList = L.filter isBlockConstraint $ concatMap permutations (L.filter (\n -> 5 == L.length n) (subsequences [1..15]))

blockArray :: [BlockPosition]
blockArray = L.map (\xs -> listArray (Red,Black) xs) blockList

compareResult :: (Int, Float, Path, BlockPosition) -> (Int, Float, Path, BlockPosition) -> Ordering
compareResult (r1,d1,p1,bp1) (r2,d2,p2,bp2)
    | r1 < r2 = GT
    | r2 < r1 = LT
    | d1 < d2 = LT
    | d2 < d1 = GT
    | otherwise = compare (p1,bp1) (p2,bp2)

calcAnyRoot :: Int -> Int -> BlockPosition -> [(Int,Float,[Node],BlockPosition)]
calcAnyRoot sp ep bp = 
    let roots = calcOptimizedRoot graph_nodes graph_edges bp (StartPoint sp) (EndPoint ep) in
    let results = sortBy compareResult (L.map (\(n,d,bp) -> (calcBonusPoint graph_nodes bp,d,n,bp) ) roots) in
    L.map head $ groupBy (\(a,_,_,_) -> \(b,_,_,_) -> a == b) results

dummyArray :: Array Int (Maybe Float)
dummyArray = array (1,25) [(i,Nothing) | i <- [1..25]]

generateRootCSVLine :: Int -> Int -> BlockPosition -> [String]
generateRootCSVLine sp ep bp = 
    let xs = calcAnyRoot sp ep bp in
    let ys = L.map (\(a,b,_,_) -> (a, Just b)) xs in
    let arr = dummyArray // ys in
    (show bp) : (L.map (maybe "" show) (A.elems arr))

main :: IO ()
main = do
    writeFile "result.csv" $ printCSV $ L.map (generateRootCSVLine 10 11) blockArray
--    print $ calcAnyRoot 10 11 $ head blockArray