module Lib (StartPoint(..), EndPoint(..), processBlockTarget, solveTarget, calcOptimizedRootTarget, getAnswerList, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode) where

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
import BinaryData
import InitCode
import BonusPoint
import GraphConstants

type BlockGraph = Gr BlockColor Float

data PointsOfBlock = PointsOfBlock (Set Node)
data FloorDirectedEdges = FloorDirectedEdges [LEdge Float]

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

append_graph_edges :: FloorUnDirectedEdges -> FloorUnDirectedEdges -> FloorUnDirectedEdges
append_graph_edges (FloorUnDirectedEdges l) (FloorUnDirectedEdges r) = FloorUnDirectedEdges (l++r)

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges (FloorUnDirectedEdges edges) =
    FloorDirectedEdges $ concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createGraph :: FloorNodes -> FloorUnDirectedEdges -> BlockGraph
createGraph (FloorNodes nodes) unDirectedEdges = 
    let (FloorDirectedEdges directedEdges) = convertDirectedEdges unDirectedEdges in
    mkGraph nodes directedEdges

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

answerList :: [[(BonusPoint,BlockPosition)]]
answerList = reverse $ groupBy (\(l,_)-> \(r,_)-> l==r) $ sort $ L.map (\n -> (calcBonusPoint graph_nodes n,n)) blockArrayRaw

isInitBlockPosition :: BlockPosition -> Bool
isInitBlockPosition bp = 
    let checkedColor = all (\(c,i) -> node_color_map M.! i /= c) (A.assocs bp) in
    let checkedDuplicate = S.size (S.fromList (A.elems bp)) == 5 in
    checkedColor && checkedDuplicate

isBlockConstraint :: [Int] -> Bool
isBlockConstraint (r:g:b:y:_) = (node_color_map M.! r /= Red) && (node_color_map M.! g /= Green) && (node_color_map M.! b /= Blue) && (node_color_map M.! y /= Yellow)
isBlockConstraint xs = False

getAnswerList :: StartPoint -> EndPoint -> BlockPosition -> BonusPoint -> Maybe (BonusPoint, Float, Path, BlockPosition)
getAnswerList sp ep bp n = maybe Nothing (\l -> let bplist = L.map snd l in f bplist n) (L.find (\xs -> fst (head xs) == n) answerList)
    where
        f :: [BlockPosition] -> BonusPoint -> Maybe (BonusPoint,Float,Path,BlockPosition)
        f xs n = maybe Nothing (\(a,b,c) -> Just (n,b,a,c)) (calcOptimizedRootTarget graph_nodes graph_edges bp xs sp ep) 

calcTargetRoot :: StartPoint -> EndPoint -> BlockPosition -> [(BonusPoint,Float,Path,BlockPosition)]
calcTargetRoot sp ep bp = catMaybes [getAnswerList sp ep bp (BonusPoint 23)] -- getAnswerList sp ep bp 23, getAnswerList sp ep bp 21, getAnswerList sp ep bp 20 ]

createRootFromCode :: Node -> Float -> InitCode -> [Word8]
createRootFromCode gp cost code = 
    let bp = fromInitCode gp code in 
    if isInitBlockPosition bp then g cost (calcTargetRoot (StartPoint 17) (EndPoint 18) bp) else []
        where
            g :: Float -> [(BonusPoint,Float,[Node],BlockPosition)] -> [Word8]
            g cost xs = 
                let fs = L.filter (\(_,c,_,_) -> c <= cost) xs in 
                if L.null fs then [] else let (_,_,r,_) = head (sort fs) in L.map fromIntegral r
        
createBinary :: Node -> Float -> BinaryData
createBinary greenPos maxCost = 
    let bps = L.map (\n -> (n,createRootFromCode greenPos maxCost n)) allInitCode in
    BinaryData $ array (minBound, maxBound) bps

