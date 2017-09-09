module Main where

import Data.List as L
import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.Binary
import Data.Maybe
import Text.CSV
import Data.Graph.Inductive.Graph

import Lib
import BlockColor
import FloorNodes
import BinaryData

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

dummyArray :: Array Int (Maybe Float)
dummyArray = array (20,25) [(i,Nothing) | i <- [20..25]]

generateRootCSVLine :: (StartPoint -> EndPoint -> BlockPosition -> [(Int,Float,[Node],BlockPosition)]) -> StartPoint -> EndPoint -> BlockPosition -> [String]
generateRootCSVLine f sp ep bp = 
    let xs = f sp ep bp in
    let ys = L.map (\(a,b,_,_) -> (a, Just b)) xs in
    let arr = dummyArray // ys in
    (show (A.elems bp)) : (L.map (maybe "" show) (A.elems arr))

main :: IO ()
main = do
    writeFile "result.csv" $ printCSV $ L.map (generateRootCSVLine calcTargetRoot (StartPoint 17) (EndPoint 18)) blockArray
--    print $ createBinary 1 4000.0
--    encodeFile "root.bin" $ createBinary 1 4000.0
--    print $ calcTargetRoot 10 11 $ blockArray !! 2
--    writeFile "answer.csv" $ printCSV answerListCSV
--    print $ L.map length $ groupBy (\(_:l:_)-> \(_:r:_)-> l==r) $ sortBy (\(_:l:_)-> \(_:r:_)-> compare l r) blockList