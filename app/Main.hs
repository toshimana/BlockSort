module Main where

import Data.List
import Data.Array

import Lib
import BlockColor
import FloorNodes
import StartPoint
import EndPoint

main :: IO ()
main = do
    let bns = array (Red, Black) [(Red,1),(Green,2),(Blue,3),(Yellow,4),(Black,5)]
    let roots = calcOptimizedRoot graph_nodes graph_edges bns (StartPoint 10) (EndPoint 11)
    print $ take 10 $ reverse $ sort (map (\(n,bp) -> (calcBonusPoint graph_nodes bp,n,bp) ) roots)
