module Main where

import Data.List as L
import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.Binary
import Data.Maybe
import Text.Printf
import Text.CSV
import Data.Graph.Inductive.Graph

import Lib
import BlockColor
import BinaryData
import BonusPoint
import GraphConstants

answerListCSV :: [[String]]
answerListCSV = L.map f answerList
    where
        f :: [(BonusPoint,BlockPosition)] -> [String]
        f xs = (show (fst (head xs))) : (show (length xs)) : (L.map g xs)
        g :: (BonusPoint,BlockPosition) -> String
        g (_,bp) = show (A.elems bp)

compareResult :: (Int, Cost, Path, BlockPosition) -> (Int, Cost, Path, BlockPosition) -> Ordering
compareResult (r1,d1,p1,bp1) (r2,d2,p2,bp2)
    | r1 < r2 = GT
    | r2 < r1 = LT
    | d1 < d2 = LT
    | d2 < d1 = GT
    | otherwise = compare (p1,bp1) (p2,bp2)

dummyArray :: Array BonusPoint (Maybe Cost)
dummyArray = array (BonusPoint 20,BonusPoint 25) [(BonusPoint i,Nothing) | i <- [20..25]]

generateRootCSVLine :: (StartPoint -> EndPoint -> BlockPosition -> [(BonusPoint,Cost,[Node],BlockPosition)]) -> StartPoint -> EndPoint -> BlockPosition -> [String]
generateRootCSVLine f sp ep bp = 
    let xs = f sp ep bp in
    let ys = L.map (\(a,b,_,_) -> (a, Just b)) xs in
    let arr = dummyArray // ys in
    (show (A.elems bp)) : (L.map (maybe "" show) (A.elems arr))

greenBlocks :: [Node]
greenBlocks = L.filter (\n -> node_color_map M.! n /= Green) [1..15]


writeAllbin id = let file = printf "root%02d.bin" id in encodeFile file $ createBinary id 100

main :: IO ()
main = do
--    writeFile "result.csv" $ printCSV $ L.map (generateRootCSVLine calcTargetRoot (StartPoint 10) (EndPoint 11)) blockArray
    print $ createBinary 9 (Cost 100.0)
--    encodeFile "root.bin" $ createBinary 9 (Cost 100.0)
--    mapM_ writeAllbin greenBlocks
--    print $ calcTargetRoot 10 11 $ blockArray !! 2
--    writeFile "answer.csv" $ printCSV answerListCSV
--    print $ L.map length $ groupBy (\(_:l:_)-> \(_:r:_)-> l==r) $ sortBy (\(_:l:_)-> \(_:r:_)-> compare l r) blockList