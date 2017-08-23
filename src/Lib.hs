module Lib (BlockPosition, node_color_map, graph_nodes, graph_edges, calcBonusPoint, solve, calcOptimizedRoot) where

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
type BlockGraph = Gr BlockColor Float

data PointsOfBlock = PointsOfBlock (Set Node)
data FloorUnDirectedEdges = FloorUnDirectedEdges [LEdge Float]
data FloorDirectedEdges = FloorDirectedEdges [LEdge Float]

node_color_list :: [(Node, BlockColor)]
node_color_list = [(1,Red),(2,Blue),(3,Yellow),(4,Blue),(5,Yellow),(6,Green),(7,Red),(8,Red),(9,Blue),(10,Green),(11,Green),(12,Blue),(13,Yellow),(14,Red),(15,Yellow), (16,None)]

node_color_map :: Map Node BlockColor
node_color_map = M.fromList node_color_list

graph_nodes :: FloorNodes
graph_nodes = FloorNodes node_color_list

graph_edge_list :: [LEdge Float]
graph_edge_list = [(1,2,77.942),(1,5,45.0),(1,10,63.64),(2,3,77.942),(2,5,45.0),(2,6,45.0),(3,4,77.942),(3,6,45.0),(3,7,45.0),(4,7,45.0),(4,11,63.64),(5,8,45.0),(5,10,45.0),(6,8,45.0),(6,9,45.0),(7,9,45.0),(7,11,45.0),(8,12,45.0),(8,13,45.0),(9,14,45.0),(9,15,45.0),(10,12,45.0),(11,15,45.0),(12,13,45.0),(13,14,32.942),(14,15,45.0)]

graph_edge_with_center_list :: [LEdge Float]
graph_edge_with_center_list = [(6,16,45),(8,16,45),(9,16,45),(13,16,45),(14,16,45)]

graph_edges :: FloorUnDirectedEdges
graph_edges = FloorUnDirectedEdges graph_edge_list

graph_edges_with_center :: FloorUnDirectedEdges
graph_edges_with_center = FloorUnDirectedEdges graph_edge_with_center_list

append_graph_edges :: FloorUnDirectedEdges -> FloorUnDirectedEdges -> FloorUnDirectedEdges
append_graph_edges (FloorUnDirectedEdges l) (FloorUnDirectedEdges r) = FloorUnDirectedEdges (l++r)

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges (FloorUnDirectedEdges edges) =
    FloorDirectedEdges $ concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createGraph :: FloorNodes -> FloorUnDirectedEdges -> BlockGraph
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
            

calcCenterBlockBonus :: BlockPosition -> Int
calcCenterBlockBonus bp = if bp A.! Black == 16 then 5 else 0

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
    let centerBlockBonus = calcCenterBlockBonus bp in
    let figureBonus = calcFigureBonus bp in
    polygonBlockBonus + centerBlockBonus + figureBonus

cuttingEdge :: FloorUnDirectedEdges -> PointsOfBlock -> FloorUnDirectedEdges
cuttingEdge (FloorUnDirectedEdges ude) (PointsOfBlock poe) =
    FloorUnDirectedEdges $ L.filter (\(l,r,_) -> not((S.member l poe) || (S.member r poe))) ude

cuttingNodes :: FloorNodes -> PointsOfBlock -> FloorNodes
cuttingNodes (FloorNodes fn) (PointsOfBlock poe) =
    FloorNodes $ L.filter (\(n,_) -> not (S.member n poe)) fn

searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> (Path, Float)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    (sp startPoint endPoint g, spLength startPoint endPoint g)

gotoend :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
gotoend fn ude bp startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (A.elems bp) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    let (path, distance) = searchShortPath startPoint endPoint g in
    [(path, distance, bp)]

getColorNode :: FloorNodes -> BlockColor -> [Node]
getColorNode (FloorNodes fn) Black = L.map (\(e,_) -> e) fn
getColorNode (FloorNodes fn) color = 
    L.foldl' (\cur -> \(e,c) -> if c == color then e:cur else cur) [] fn

processBlock :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
processBlock fn ude bp cl bc bcn startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (L.delete bcn (A.elems bp)) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    let (departRoot, departDistance) = searchShortPath startPoint (EndPoint bcn) g in
    if L.null departRoot then []
    else
        let noblock_nodes = cuttingNodes fn poe in
        let target_nodes = getColorNode noblock_nodes bc in
        searchCenter fn ude poe departRoot departDistance bc bcn ++ L.concatMap (searchRoundRoot g departRoot departDistance) target_nodes
        where
            f :: Node -> Node -> [(Path,Float,BlockPosition)]
            f e backNode = solve fn ude (bp // [(bc,e)]) cl (StartPoint backNode) endPoint
            searchRoundRoot :: BlockGraph -> [Node] -> Float -> Node -> [(Path,Float,BlockPosition)]
            searchRoundRoot g departRoot departDistance e = 
                let (returnRoot, returnDistance) = searchShortPath (StartPoint bcn) (EndPoint e) g in
                if L.null returnRoot then []
                else 
                    let moveRoot = departRoot ++ returnRoot in
                    let moveDistance = departDistance + returnDistance in
                    let backNode = last (init moveRoot) in
                    let currentRoot = moveRoot ++ [backNode] in
                    let currentDistance = moveDistance + (spLength e backNode g) in
                    B.mapMaybe (\(path,distance,newbp) -> if L.null path then Nothing else Just (currentRoot ++ path, currentDistance + distance, newbp)) (f e backNode)
            searchCenter :: FloorNodes -> FloorUnDirectedEdges -> PointsOfBlock -> [Node] -> Float -> BlockColor -> Node -> [(Path,Float,BlockPosition)]
            searchCenter fn ude poe departRoot departDistance Black n = 
                let noblock_ude = cuttingEdge (append_graph_edges ude graph_edges_with_center) poe in
                let g = createGraph fn noblock_ude in
                searchRoundRoot g departRoot departDistance 16
            searchCenter _ _ _ _ _ _ _ = []

solve :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
solve fn ude bp [] startPoint endPoint = gotoend fn ude bp startPoint endPoint
solve fn ude bp unprocessBlocks startPoint endPoint = 
    concatMap (\p@(color,node) -> processBlock fn ude bp (L.delete p unprocessBlocks) color node startPoint endPoint) unprocessBlocks

calcOptimizedRoot :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
calcOptimizedRoot fn ude bp startPoint endPoint = 
    let bplist = A.assocs bp in
    solve fn ude bp bplist startPoint endPoint
