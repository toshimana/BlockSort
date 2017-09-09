module BonusPoint (calcBonusPoint) where

import Data.List as L
import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.Graph.Inductive.Graph

import BlockColor
import GraphConstants

calcPolygonBlockBonus :: FloorNodes -> BlockPosition -> Int
calcPolygonBlockBonus bn bp = L.foldl' checkColor 0 colorNodeList
        where
            colorNodeList :: [(BlockColor, Node)]
            colorNodeList = A.assocs bp
            checkColor :: Int -> (BlockColor, Node) -> Int
            checkColor cur (color, node) = if color == getNodeColor node then cur + 2 else cur
            getNodeColor :: Node -> BlockColor
            getNodeColor n = (M.!) node_color_map n

calcCenterBlockBonus :: BlockPosition -> Int
calcCenterBlockBonus bp = if bp A.! Black == 16 then 5 else 0

convertNodesToColorNode :: [[Node]] -> [Set (BlockColor, Node)]
convertNodesToColorNode = L.map (S.fromList . (L.map (\n -> ((M.!) node_color_map n, n))))

calcFigureBonusImpl ::Int -> [Set (BlockColor,Node)] -> Set (BlockColor,Node) -> Int
calcFigureBonusImpl bonus target bs =
    L.foldl' (\cur -> \elt -> if isSubsetOf elt bs then cur+bonus else cur) 0 target

triangles :: [Set (BlockColor,Node)]
triangles = convertNodesToColorNode [[1,2,5],[1,5,10],[2,3,6],[3,4,7],[4,7,11],[8,12,13],[9,14,15]]

calcTriangleBonus :: Set (BlockColor,Node) -> Int
calcTriangleBonus = calcFigureBonusImpl 5 triangles

depressionSquare :: [Set (BlockColor,Node)]
depressionSquare = convertNodesToColorNode [[1,2,5,10],[3,4,7,11]]

calcDepressionSquareBonus :: Set (BlockColor,Node) -> Int
calcDepressionSquareBonus = calcFigureBonusImpl 2 depressionSquare

square :: [Set (BlockColor,Node)]
square = convertNodesToColorNode [[2,5,6,8],[3,6,7,9],[5,8,10,12],[7,9,11,15]]

calcSquareBonus :: Set (BlockColor,Node) -> Int
calcSquareBonus = calcFigureBonusImpl 8 square

pentagon :: [Set (BlockColor,Node)]
pentagon = [S.fromList [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)], S.fromList [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]]

calcPentagonBonus :: Set (BlockColor,Node) -> Int
calcPentagonBonus = calcFigureBonusImpl 15 pentagon

calcFigureBonus :: BlockPosition -> Int
calcFigureBonus bp = 
    let blockSet = S.fromList (A.assocs bp) in
    let triangleBonus = calcTriangleBonus blockSet in
    let depressionSquareBonus = calcDepressionSquareBonus blockSet in
    let squareBonus = calcSquareBonus blockSet in
    let pentagonBonus = calcPentagonBonus blockSet in
    triangleBonus + depressionSquareBonus + squareBonus + pentagonBonus

calcBonusPoint :: FloorNodes -> BlockPosition -> Int
calcBonusPoint bn bp = 
    let polygonBlockBonus = calcPolygonBlockBonus bn bp in
    let centerBlockBonus = calcCenterBlockBonus bp in
    let figureBonus = calcFigureBonus bp in
    polygonBlockBonus + centerBlockBonus + figureBonus