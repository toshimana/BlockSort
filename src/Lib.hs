module Lib (BlockPosition, node_color_map, graph_nodes, graph_edges, toInitCode, fromInitCode, calcBonusPoint, processBlockTarget, solveTarget, calcOptimizedRootTarget, getAnswerList, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode) where

import Data.Array as A
import Data.Map as M
import Data.Set as S
import Data.List as L
import Data.Maybe as B
import Data.Word
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP

import BlockColor
import FloorNodes
import StartPoint
import EndPoint
import BinaryData
import InitCode

type BlockPosition = Array BlockColor Node
type BlockGraph = Gr BlockColor Float

data PointsOfBlock = PointsOfBlock (Set Node)
data FloorUnDirectedEdges = FloorUnDirectedEdges [LEdge Float]
data FloorDirectedEdges = FloorDirectedEdges [LEdge Float]

node_color_list :: [(Node, BlockColor)]
node_color_list = [(1,Red),(2,Blue),(3,Yellow),(4,Blue),(5,Yellow),(6,Green),(7,Red),(8,Red),(9,Blue),(10,Green),(11,Green),(12,Blue),(13,Yellow),(14,Red),(15,Yellow), (16,None)]

middle_node_list :: [(Node, BlockColor)]
middle_node_list = L.map (\n -> (n,None)) [17..44]

node_color_map :: Map Node BlockColor
node_color_map = M.fromList node_color_list

graph_nodes :: FloorNodes
-- graph_nodes = FloorNodes node_color_list
graph_nodes = FloorNodes $ node_color_list ++ middle_node_list

l1 = 77.942
l2 = 63.64
l3 = 45.0
l4 = 32.942

graph_edge_list :: [LEdge Float]
graph_edge_list = [(1,2,l1),(1,5,l3),(1,10,l2),(2,3,l1),(2,5,l3),(2,6,l3),(3,4,l1),(3,6,l3),(3,7,l3),(4,7,l3),(4,11,l2),(5,8,l3),(5,10,l3),(6,8,l3),(6,9,l3),(7,9,l3),(7,11,l3),(8,12,l3),(8,13,l3),(9,14,l3),(9,15,l3),(10,12,l3),(11,15,l3),(12,13,l3),(13,14,l4),(14,15,l3)]

h1 = l1 / 2.0
h2 = l2 / 2.0
h3 = l3 / 2.0
h4 = l4 / 2.0
h5 = 67.768 / 2.0

graph_middle_edge_list :: [LEdge Float]
graph_middle_edge_list = 
    [(1,19,h1),(1,22,h3),(1,28,h2)
    ,(2,19,h1),(2,20,h1),(2,23,h3),(2,24,h3)
    ,(3,20,h1),(3,21,h1),(3,25,h3),(3,26,h3)
    ,(4,21,h1),(4,27,h3),(4,29,h2)
    ,(5,22,h3),(5,23,h3),(5,30,h3),(5,34,h3)
    ,(6,24,h3),(6,25,h3),(6,31,h3),(6,32,h3)
    ,(7,26,h3),(7,27,h3),(7,33,h3),(7,35,h3)
    ,(8,30,h3),(8,31,h3),(8,36,h3),(8,37,h3)
    ,(9,32,h3),(9,33,h3),(9,38,h3),(9,39,h3)
    ,(10,28,h2),(10,34,h3),(10,40,h3)
    ,(11,29,h2),(11,35,h3),(11,41,h3)
    ,(12,36,h3),(12,40,h3),(12,42,h3)
    ,(13,37,h3),(13,42,h3),(13,43,h4)
    ,(14,38,h3),(14,43,h4),(14,44,h3)
    ,(15,39,h3),(15,41,h3),(15,44,h3)
    ,(17,10,19.15)
    ,(18,11,40.17)]

graph_middle_middle_edge_list :: [LEdge Float]
graph_middle_middle_edge_list = 
    [(17,28,47.34),(17,40,29.546)
    ,(18,29,34.655)
    ,(19,22,h3),(19,23,h3)
    ,(20,24,h3),(20,25,h3)
    ,(21,26,h3),(21,27,h3)
    ,(22,23,h1),(22,28,h3),(22,34,h2)
    ,(23,24,h1),(23,30,h3)
    ,(24,25,h1),(24,31,h3)
    ,(25,26,h1),(25,32,h3)
    ,(26,27,h1),(26,33,h3)
    ,(27,29,h3),(27,35,h2)
    ,(28,34,h3)
    ,(29,35,h3)
    ,(30,31,h1),(30,34,h2),(30,36,h2)
    ,(31,32,h1),(31,37,h2)
    ,(32,33,h1),(32,38,h2)
    ,(33,35,h2),(33,39,h2)
    ,(34,40,h2)
    ,(35,41,h2)
    ,(36,37,h3),(36,40,h2),(36,42,h3)
    ,(37,42,h3),(37,43,h5)
    ,(38,39,h3),(38,43,h5),(38,44,h3)
    ,(39,41,h2)]

graph_edge_with_center_list :: [LEdge Float]
graph_edge_with_center_list = [(6,16,45),(8,16,45),(9,16,45),(13,16,45),(14,16,45)]

graph_edges :: FloorUnDirectedEdges
-- graph_edges = FloorUnDirectedEdges graph_edge_list
graph_edges = FloorUnDirectedEdges $ graph_middle_edge_list ++ graph_middle_middle_edge_list

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

isDeadLock :: [Node] -> [(BlockColor, Node)] -> BlockPosition -> Node -> Bool
isDeadLock checked cl bp e = if L.elem e checked then True
    else let nodes = L.filter (\n -> snd n == e) (A.assocs bp) in 
        if L.null nodes then False
        else let target = head nodes in
            let next = L.find (\(c,n) -> fst target == c) cl in
            maybe False (\(_,n) -> isDeadLock (e:checked) cl bp n) next

findEscapeNode :: BlockGraph -> BlockPosition -> [(BlockColor, Node)] -> Node -> (Path, Float)
findEscapeNode g bp cl s = let onboardnodes = A.elems bp in 
    let targetnodes = L.map snd cl in
    let checknodes = onboardnodes ++ targetnodes in
    let nodes = L.filter (\n -> not (L.elem n checknodes)) [1..15] in
    let ms = catMaybes $ L.map (\e -> searchShortPath (StartPoint s) (EndPoint e) g) nodes in
    head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms

processBlockTarget :: FloorNodes -> FloorUnDirectedEdges -> BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Float,BlockPosition)]
processBlockTarget fn ude bp cl bc bcn startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
    let noblock_ude = cuttingEdge ude poe in
    let g = createGraph fn noblock_ude in
    let current_block_point = bp A.! bc in
    let departEndPoint = EndPoint current_block_point in
    if current_block_point == bcn then solveTarget fn ude bp cl startPoint endPoint
    else case searchShortPath startPoint departEndPoint g of
            Nothing -> []
            Just (departRoot, departCost) ->
                if (bc,bcn) == (Black,16) then searchCenter fn ude poe departRoot departCost current_block_point 
                else searchRoundRoot g departRoot departCost current_block_point bcn
                where
                    f :: Node -> Node -> [(Path,Float,BlockPosition)]
                    f e backNode = solveTarget fn ude (bp // [(bc,e)]) cl (StartPoint backNode) endPoint
                    searchRoundRoot :: BlockGraph -> [Node] -> Float -> Node -> Node -> [(Path,Float,BlockPosition)]
                    searchRoundRoot g departRoot departCost s e = let currentCl = (bc,e):cl in
                        if isDeadLock [] currentCl bp e
                        then let (returnRoot, returnCost) = findEscapeNode g bp currentCl s in
                                let moveRoot = departRoot ++ (tail returnRoot) in
                                let moveCost = departCost + returnCost in
                                let escapeNode = last moveRoot in
                                let backNode = last (init moveRoot) in
                                let currentRoot = moveRoot ++ [backNode] in
                                let currentCost = maybe 0 ((+) moveCost) (spLength e backNode g) in
                                let restSolve = solveTarget fn ude (bp // [(bc,escapeNode)]) ((bc,bcn):cl) (StartPoint backNode) endPoint in
                                B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) restSolve
                        else searchRoundRoot_ g departRoot departCost s e
                    searchRoundRoot_ :: BlockGraph -> [Node] -> Float -> Node -> Node -> [(Path,Float,BlockPosition)]
                    searchRoundRoot_ g departRoot departCost s e = 
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
                        searchRoundRoot_ g departRoot departCost s 16

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

isInitBlockPosition :: BlockPosition -> Bool
isInitBlockPosition bp = 
    let checkedColor = all (\(c,i) -> node_color_map M.! i /= c) (A.assocs bp) in
    let checkedDuplicate = S.size (S.fromList (A.elems bp)) == 5 in
    checkedColor && checkedDuplicate

isBlockConstraint :: [Int] -> Bool
isBlockConstraint (r:g:b:y:_) = (node_color_map M.! r /= Red) && (node_color_map M.! g /= Green) && (node_color_map M.! b /= Blue) && (node_color_map M.! y /= Yellow)
isBlockConstraint xs = False

getAnswerList :: StartPoint -> EndPoint -> BlockPosition -> Int -> Maybe (Int, Float, [Node], BlockPosition)
getAnswerList sp ep bp n = maybe Nothing (\l -> let bplist = L.map snd l in f bplist n) (L.find (\xs -> fst (head xs) == n) answerList)
    where
        f :: [BlockPosition] -> Int -> Maybe (Int,Float,[Node],BlockPosition)
        f xs n = maybe Nothing (\(a,b,c) -> Just (n,b,a,c)) (calcOptimizedRootTarget graph_nodes graph_edges bp xs sp ep) 

calcTargetRoot :: StartPoint -> EndPoint -> BlockPosition -> [(Int,Float,[Node],BlockPosition)]
calcTargetRoot sp ep bp = catMaybes [getAnswerList sp ep bp 23] -- getAnswerList sp ep bp 23, getAnswerList sp ep bp 21, getAnswerList sp ep bp 20 ]

createRootFromCode :: Node -> Float -> InitCode -> [Word8]
createRootFromCode gp cost code = 
    let bp = fromInitCode gp code in 
    if isInitBlockPosition bp then g cost (calcTargetRoot (StartPoint 17) (EndPoint 18) bp) else []
        where
            g :: Float -> [(Int,Float,[Node],BlockPosition)] -> [Word8]
            g cost xs = 
                let fs = L.filter (\(_,c,_,_) -> c <= cost) xs in 
                if L.null fs then [] else let (_,_,r,_) = head (sort fs) in L.map fromIntegral r
        
createBinary :: Node -> Float -> BinaryData
createBinary greenPos maxCost = 
    let bps = L.map (\n -> let code = InitCode n in (code,createRootFromCode greenPos maxCost code)) [0..15*11*11*11-1] in
    BinaryData $ array (InitCode 0,InitCode (15*11*11*11-1)) bps

