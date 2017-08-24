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

blockListRaw :: [[Int]]
blockListRaw = concatMap permutations (L.filter (\n -> 5 == L.length n) (subsequences [1..16]))

blockList :: [[Int]]
blockList = L.filter isBlockConstraint $ concatMap permutations (L.filter (\n -> 5 == L.length n) (subsequences [1..15]))

blockArrayRaw :: [BlockPosition]
blockArrayRaw = L.map (\xs -> listArray (Red,Black) xs) blockListRaw

blockArray :: [BlockPosition]
blockArray = L.map (\xs -> listArray (Red,Black) xs) blockList

answerList :: [[(Int,BlockPosition)]]
answerList = reverse $ groupBy (\(l,_)-> \(r,_)-> l==r) $ sort $ L.map (\n -> (calcBonusPoint graph_nodes n,n)) blockArrayRaw

answerListCSV :: [[String]]
answerListCSV = L.map f answerList
    where
        f :: [(Int,BlockPosition)] -> [String]
        f xs = (show (fst (head xs))) : (show (length xs)) : (L.map g xs)
        g :: (Int,BlockPosition) -> String
        g (_,bp) = show (A.elems bp)

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
    let bps = L.map (\(n,d,bp) -> (calcBonusPoint graph_nodes bp,d,n,bp) ) roots in
    let fs = L.map (\n -> L.filter (\(m,_,_,_) -> n == m) bps) [1..25] in
    L.map (minimumBy (\(_,l,_,_) -> \(_,r,_,_) -> compare l r)) $ L.filter (not.(L.null)) fs

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
--    writeFile "answer.csv" $ printCSV answerListCSV
