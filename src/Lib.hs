module Lib (graph_nodes, graph_edges, calcBonusPoint, solve, calcOptimizedRoot) where

import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe as B
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP

import BlockColor
import FloorNodes
import StartPoint
import EndPoint

type BlockPosition = Array BlockColor Node

data PointsOfBlock = PointsOfBlock (Set Node)
data FloorUnDirectedEdges = FloorUnDirectedEdges [LEdge Float]
data FloorDirectedEdges = FloorDirectedEdges [LEdge Float]

node_color_list :: [(Node, BlockColor)]
node_color_list = [(1,Red),(2,Blue),(3,Yellow),(4,Blue),(5,Yellow),(6,Green),(7,Red),(8,Red),(9,Blue),(10,Green),(11,Green),(12,Blue),(13,Yellow),(14,Red),(15,Yellow)]

node_color_map :: Map Node BlockColor
node_color_map = M.fromList node_color_list

graph_nodes :: FloorNodes
graph_nodes = FloorNodes node_color_list

graph_edges :: FloorUnDirectedEdges
graph_edges = FloorUnDirectedEdges [(1,2,77.942),(1,5,45.0),(1,10,63.64),(2,3,77.942),(2,5,45.0),(2,6,45.0),(3,4,77.942),(3,6,45.0),(3,7,45.0),(4,7,45.0),(4,11,63.64),(5,8,45.0),(5,10,45.0),(6,8,45.0),(6,9,45.0),(7,9,45.0),(7,11,45.0),(8,12,45.0),(8,13,45.0),(9,14,45.0),(9,15,45.0),(10,12,45.0),(11,15,45.0),(12,13,45.0),(13,14,32.942),(14,15,45.0)]

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges (FloorUnDirectedEdges edges) =
    FloorDirectedEdges $ concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createGraph :: FloorNodes -> FloorUnDirectedEdges -> Gr BlockColor Float
createGraph (FloorNodes nodes) unDirectedEdges = 
    let (FloorDirectedEdges directedEdges) = convertDirectedEdges unDirectedEdges in
    mkGraph nodes directedEdges

calcPolygonBlockBonus :: FloorNodes -> BlockPosition -> Int
calcPolygonBlockBonus bn bp = L.foldl' checkColor 0 colorNodeList
        where
            colorNodeList :: [(BlockColor, Node)]
            colorNodeList = A.assocs bp
            checkColor :: Int -> (BlockColor, Node) -> Int
            checkColor cur (color, node) = if color == getNodeColor node then cur + 2 else cur
            getNodeColor :: Node -> BlockColor
            getNodeColor n = (M.!) node_color_map n
            

calcCenterBlockBonus :: FloorNodes -> BlockPosition -> Int
calcCenterBlockBonus bn bp = 0

calcFigureBonusImpl ::Int -> [Set Node] -> PointsOfBlock -> Int
calcFigureBonusImpl bonus target (PointsOfBlock pob) =
    L.foldl' (\cur -> \elt -> if isSubsetOf elt pob then cur+bonus else cur) 0 target


triangles :: [Set Node]
triangles = L.map S.fromList [[1,2,5],[1,5,10],[2,3,6],[3,4,7],[4,7,11],[8,12,13],[9,14,15]]

calcTriangleBonus :: PointsOfBlock -> Int
calcTriangleBonus = calcFigureBonusImpl 5 triangles

depressionSquare :: [Set Node]
depressionSquare = L.map S.fromList [[1,2,5,10],[3,4,7,11]]

calcDepressionSquareBonus :: PointsOfBlock -> Int
calcDepressionSquareBonus = calcFigureBonusImpl 2 depressionSquare

square :: [Set Node]
square = L.map S.fromList [[2,5,6,8],[3,6,7,9],[5,8,10,12],[7,9,11,15]]

calcSquareBonus :: PointsOfBlock -> Int
calcSquareBonus = calcFigureBonusImpl 8 square

pentagon :: [Set Node]
pentagon = L.map S.fromList [[6,8,9,13,14]]

calcPentagonBonus :: PointsOfBlock -> Int
calcPentagonBonus = calcFigureBonusImpl 15 pentagon

calcFigureBonus :: BlockPosition -> Int
calcFigureBonus bp = 
    let pointsOfBlock = PointsOfBlock (S.fromList (A.elems bp)) in
    let triangleBonus = calcTriangleBonus pointsOfBlock in
    let depressionSquareBonus = calcDepressionSquareBonus pointsOfBlock in
    let squareBonus = calcSquareBonus pointsOfBlock in
    let pentagonBonus = calcPentagonBonus pointsOfBlock in
    triangleBonus + depressionSquareBonus + squareBonus + pentagonBonus

calcBonusPoint :: FloorNodes -> BlockPosition -> Int
calcBonusPoint bn bp = 
    let polygonBlockBonus = calcPolygonBlockBonus bn bp in
    let centerBlockBonus = calcCenterBlockBonus bn bp in
    let figureBonus = calcFigureBonus bp in
    polygonBlockBonus + centerBlockBonus + figureBonus

cuttingEdge :: FloorUnDirectedEdges -> PointsOfBlock -> FloorUnDirectedEdges
cuttingEdge (FloorUnDirectedEdges ude) (PointsOfBlock poe) =
    FloorUnDirectedEdges $ L.filter (\(l,r,_) -> not((S.member l poe) || (S.member r poe))) ude

cuttingNodes :: FloorNodes -> PointsOfBlock -> FloorNodes
cuttingNodes (FloorNodes fn) (PointsOfBlock poe) =
    FloorNodes $ L.filter (\(n,_) -> not (S.member n poe)) fn

searchShortPath :: StartPoint -> EndPoint -> Gr BlockColor Float -> [Node]
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g = sp startPoint endPoint g

gotoend :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> StartPoint -> EndPoint -> [([Node],BlockPosition)]
gotoend fn ude bp startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (A.elems bp) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    [(searchShortPath startPoint endPoint g, bp)]

getColorNode :: FloorNodes -> BlockColor -> [Node]
getColorNode (FloorNodes fn) Black = L.map (\(e,_) -> e) fn
getColorNode (FloorNodes fn) color = 
    L.foldl' (\cur -> \(e,c) -> if c == color then e:cur else cur) [] fn

processBlock :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [([Node],BlockPosition)]
processBlock fn ude bp cl bc bcn startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (L.delete bcn (A.elems bp)) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    let departRoot = searchShortPath startPoint (EndPoint bcn) g in
    if L.null departRoot then []
    else
        let noblock_nodes = cuttingNodes fn poe in
        let target_nodes = getColorNode noblock_nodes bc in
        L.concatMap (\e -> B.mapMaybe (searchReturnRoot g departRoot e) (f e) ) target_nodes
        where
            f :: Node -> [([Node],BlockPosition)]
            f e = solve fn ude (bp // [(bc,e)]) cl (StartPoint e) endPoint
            searchReturnRoot :: Gr BlockColor Float -> [Node] -> Node -> ([Node], BlockPosition) -> Maybe ([Node], BlockPosition)
            searchReturnRoot g departRoot e (path, newbp) =
                if L.null path then Nothing
                else
                    let returnRoot = searchShortPath (StartPoint bcn) (EndPoint e) g in
                    if L.null returnRoot then Nothing
                    else Just (departRoot ++ returnRoot ++ path, newbp)

solve :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> StartPoint -> EndPoint -> [([Node],BlockPosition)]
solve fn ude bp [] startPoint endPoint = gotoend fn ude bp startPoint endPoint
solve fn ude bp unprocessBlocks startPoint endPoint = 
    concatMap (\p@(color,node) -> processBlock fn ude bp (L.delete p unprocessBlocks) color node startPoint endPoint) unprocessBlocks

calcOptimizedRoot :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> StartPoint -> EndPoint -> [([Node],BlockPosition)]
calcOptimizedRoot fn ude bp startPoint endPoint = 
    let bplist = A.assocs bp in
    solve fn ude bp bplist startPoint endPoint
