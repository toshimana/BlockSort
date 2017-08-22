module Main where

import Data.List
import Data.Array
import Data.Graph.Inductive.Graph

import Lib
import BlockColor
import FloorNodes
import StartPoint
import EndPoint

compareResult :: (Int, Float, Path, BlockPosition) -> (Int, Float, Path, BlockPosition) -> Ordering
compareResult (r1,d1,p1,bp1) (r2,d2,p2,bp2)
    | r1 < r2 = GT
    | r2 < r1 = LT
    | d1 < d2 = LT
    | d2 < d1 = GT
    | otherwise = compare (p1,bp1) (p2,bp2)

main :: IO ()
main = do
    let bns = array (Red, Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,5)]
    let roots = calcOptimizedRoot graph_nodes graph_edges bns (StartPoint 10) (EndPoint 11)
    let results = sortBy compareResult (map (\(n,d,bp) -> (calcBonusPoint graph_nodes bp,d,n,bp) ) roots)
    print $ map head $ groupBy (\(a,_,_,_) -> \(b,_,_,_) -> a == b) results
