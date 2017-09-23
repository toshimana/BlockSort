module Lib (StartPoint(..), EndPoint(..), processBlockTarget, addMiniEdges, addParentEdges, goto, createGraph, searchShortPath, solveTarget, calcOptimizedRootTarget, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode, targetList) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Array as A
import qualified Data.Map as M
import Data.Word
import Data.Graph.Inductive.Tree
import Data.Graph.Inductive.Graph

import BlockColor
import BinaryData
import InitCode
import BonusPoint
import Graph
import GraphConstants
import Cost
import SolveGraph

calcOptimizedRootTarget :: BlockPosition -> [BlockPosition] -> StartPoint -> EndPoint -> Maybe (Path,Cost,BlockPosition)
calcOptimizedRootTarget bp tblist startPoint endPoint = 
    let ans = solveTarget bp [Red .. Black] tblist startPoint endPoint in
    if L.null ans then Nothing else Just (L.minimumBy (\(_,l,_) -> \(_,r,_) -> compare l r) ans)
        
blockListRaw :: [[Int]]
blockListRaw = L.concatMap L.permutations (L.filter (\n -> 5 == L.length n) (L.subsequences [1..16]))

blockList :: [[Int]]
blockList = L.filter isBlockConstraint $ L.concatMap L.permutations (L.filter (\n -> 5 == L.length n) (L.subsequences [1..15]))

blockArrayRaw :: [BlockPosition]
blockArrayRaw = L.map createBlockPosition blockListRaw

blockArray :: [BlockPosition]
blockArray = L.map createBlockPosition blockList

answerList :: [[(BonusPoint,BlockPosition)]]
answerList = L.reverse $ L.groupBy (\(l,_)-> \(r,_)-> l==r) $ L.sort $ L.map (\n -> (calcBonusPoint graph_nodes n,n)) blockArrayRaw

isInitBlockPosition :: BlockPosition -> Bool
isInitBlockPosition bp = 
    let checkedColor = all (\(c,i) -> node_color_map M.! i /= c) (A.assocs bp) in
    let checkedDuplicate = S.size (S.fromList (A.elems bp)) == 5 in
    checkedColor && checkedDuplicate

isBlockConstraint :: [Int] -> Bool
isBlockConstraint (r:g:b:y:_) = (node_color_map M.! r /= Red) && (node_color_map M.! g /= Green) && (node_color_map M.! b /= Blue) && (node_color_map M.! y /= Yellow)
isBlockConstraint xs = False

isTargetFigure :: BlockPosition -> Bool
isTargetFigure a = (not (isCenterBlockBonus a)) && ((isDepressionSquareBonus a) || (isSquareBonus a) || (isPentagonBonus a))


targetList :: BlockPosition -> [BlockPosition]
targetList bp = let l = L.filter isTargetFigure blockArrayRaw in
    let replaceList = L.map (\n -> if not (isPentagonBonus n) then n A.// [(Black,bp A.! Black)] else n) l in
    let blockPlaceFilterList = L.filter (\n -> let ns = A.elems n in S.size (S.fromList ns) == 5) replaceList in
    S.toList $ S.fromList blockPlaceFilterList

calcTargetRoot :: StartPoint -> EndPoint -> BlockPosition -> [(BonusPoint,Cost,Path,BlockPosition)]
calcTargetRoot sp ep bp = f (targetList bp)
    where 
        f :: [BlockPosition] -> [(BonusPoint,Cost,Path,BlockPosition)]
        f tlist = 
            let ans = solveTarget bp [Red .. Black] tlist sp ep in
            L.map (\(p,c,bp) -> (calcBonusPoint graph_nodes bp,c,init (tail (refinePath miniNodeToParentNode p)),bp)) ans

createRootFromCode :: Node -> Cost -> InitCode -> [Word8]
createRootFromCode gp cost code = 
    let bp = fromInitCode gp code in 
    if isInitBlockPosition bp 
    then g cost (calcTargetRoot (StartPoint 17) (EndPoint 18) bp) 
    else []
        where
            g :: Cost -> [(BonusPoint,Cost,Path,BlockPosition)] -> [Word8]
            g cost xs = 
                let fs = L.filter (\(_,c,_,_) -> c <= cost) xs in 
                if L.null fs then [] else let (_,_,r,_) = head (L.sort fs) in L.map fromIntegral r
        
createBinary :: Node -> Cost -> BinaryData
createBinary greenPos maxCost = 
    let bps = L.map (\n -> (n,createRootFromCode greenPos maxCost n)) allInitCode in
    createBinaryData bps

