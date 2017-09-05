module Lib (BlockPosition, node_color_map, graph_nodes, graph_edges, toInitCode, calcBonusPoint, processBlockTarget, solveTarget, calcOptimizedRootTarget) where

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

redIndices :: [Maybe Int]
redIndices = [Nothing,Just 1,Just 2,Just 3,Just 4,Just 5,Nothing,Nothing,Just 6,Just 7,Just 8,Just 9,Just 10,Nothing,Just 11]

yellowIndices :: [Maybe Int]
yellowIndices = [Just 1,Just 2,Nothing,Just 3,Nothing,Just 4,Just 5,Just 6,Just 7,Just 8,Just 9,Just 10,Nothing,Just 11,Nothing]

blueIndices :: [Maybe Int]
blueIndices = [Just 1,Nothing,Just 2,Nothing,Just 3,Just 4,Just 5,Just 6,Nothing,Just 7,Just 8,Nothing,Just 9,Just 10,Just 11]

toInitCode :: BlockPosition -> Int
toInitCode bp = 
    let blackP = bp A.! Black in
    let redP = fromJust $ redIndices !! ((bp A.! Red)-1) in
    let yellowP = fromJust $ yellowIndices !! ((bp A.! Yellow)-1) in
    let blueP = fromJust $ blueIndices !! ((bp A.! Blue)-1) in
    (blackP-1)*11*11*11+(redP-1)*11*11+(yellowP-1)*11+(blueP-1)
    
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

cuttingEdge :: FloorUnDirectedEdges -> PointsOfBlock -> FloorUnDirectedEdges
cuttingEdge (FloorUnDirectedEdges ude) (PointsOfBlock poe) =
    FloorUnDirectedEdges $ L.filter (\(l,r,_) -> not((S.member l poe) || (S.member r poe))) ude

cuttingNodes :: FloorNodes -> PointsOfBlock -> FloorNodes
cuttingNodes (FloorNodes fn) (PointsOfBlock poe) =
    FloorNodes $ L.filter (\(n,_) -> not (S.member n poe)) fn

searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> Maybe (Path, Float)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    case (sp startPoint endPoint g, spLength startPoint endPoint g) of
        (Just path, Just cost) -> Just (path, cost)
        (_, _) -> Nothing

gotoend :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> StartPoint -> EndPoint -> Maybe (Path,Float,BlockPosition)
gotoend fn ude bp startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (A.elems bp) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    case searchShortPath startPoint endPoint g of
        Just (path, cost) -> Just (path, cost, bp)
        Nothing -> Nothing

getColorNode :: FloorNodes -> BlockColor -> [Node]
getColorNode (FloorNodes fn) Black = L.map (\(e,_) -> e) fn
getColorNode (FloorNodes fn) color = 
    L.foldl' (\cur -> \(e,c) -> if c == color then e:cur else cur) [] fn

processBlockTarget :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
processBlockTarget fn ude bp cl bc bcn startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    let current_block_point = bp A.! bc in
    case searchShortPath startPoint (EndPoint current_block_point) g of
        Nothing -> []
        Just (departRoot, departCost) ->
            if (bc,bcn) == (Black,16) then searchCenter fn ude poe departRoot departCost current_block_point 
            else searchRoundRoot g departRoot departCost current_block_point bcn
            where
                f :: Node -> Node -> [(Path,Float,BlockPosition)]
                f e backNode = solveTarget fn ude (bp // [(bc,e)]) cl (StartPoint backNode) endPoint
                searchRoundRoot :: BlockGraph -> [Node] -> Float -> Node -> Node -> [(Path,Float,BlockPosition)]
                searchRoundRoot g departRoot departCost s e = 
                    case searchShortPath (StartPoint s) (EndPoint e) g of
                        Nothing -> []
                        Just (returnRoot, returnCost) ->
                            let moveRoot = departRoot ++ (tail returnRoot) in
                            let moveCost = departCost + returnCost in
                            let backNode = last (init moveRoot) in
                            let currentRoot = moveRoot ++ [backNode] in
                            let currentCost = maybe 0 ((+) moveCost) (spLength e backNode g) in
                            B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) (f e backNode)
                searchCenter :: FloorNodes -> FloorUnDirectedEdges -> PointsOfBlock -> [Node] -> Float -> Node -> [(Path,Float,BlockPosition)]
                searchCenter fn ude poe departRoot departCost s = 
                    let noblock_ude = cuttingEdge (append_graph_edges ude graph_edges_with_center) poe in
                    let g = createGraph fn noblock_ude in
                    searchRoundRoot g departRoot departCost s 16

solveTarget :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
solveTarget fn ude bp [] startPoint endPoint = maybeToList (gotoend fn ude bp startPoint endPoint)
solveTarget fn ude bp unprocessBlocks startPoint endPoint = 
    concatMap (\p@(color,node) -> processBlockTarget fn ude bp (L.delete p unprocessBlocks) color node startPoint endPoint) unprocessBlocks

calcOptimizedRootTarget :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [BlockPosition] -> StartPoint -> EndPoint -> Maybe (Path,Float,BlockPosition)
calcOptimizedRootTarget fn ude bp tblist startPoint endPoint = 
    let ans = L.concatMap f tblist in
    if L.null ans then Nothing else Just (minimumBy (\(_,l,_) -> \(_,r,_) -> compare l r) ans)
    where
        f :: BlockPosition -> [(Path,Float,BlockPosition)]
        f tb = let bplist = A.assocs tb in solveTarget fn ude bp bplist startPoint endPoint
        

