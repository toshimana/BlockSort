module Lib (StartPoint(..), EndPoint(..), processBlockTarget, createRotateBaseEdges, addMiniEdges, addParentEdges, goto, createGraph, searchShortPath, solveTarget, calcOptimizedRootTarget, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode, targetList) where

import Data.Array as A
import Data.Map as M
import Data.MultiMap as MM
import Data.Set as S
import Data.List as L
import Data.Maybe as B
import Linear.Metric
import Data.Word
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Query.SP

import BlockColor
import BinaryData
import InitCode
import BonusPoint
import GraphConstants

type BlockGraph = Gr NodeInfo Cost

type PointsOfBlock = Set Node
type FloorDirectedEdges = [LEdge Cost]

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges edges =
    concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createRotateBaseEdges :: (FloorNodes,FloorDirectedEdges,MultiMap Node (LNode NodeInfo),MultiMap Node (LNode NodeInfo),Map Node Node,Node) -> LEdge Cost -> (FloorNodes,FloorDirectedEdges,MultiMap Node (LNode NodeInfo),MultiMap Node (LNode NodeInfo),Map Node Node,Node)
createRotateBaseEdges (fn,fde,toOuter,toInner,toParent,id) e@(n1,n2,c) = 
    let nc1 = node_color_map M.! n1 in
    let nc2 = node_color_map M.! n2 in
    let p1 = node_position_map M.! n1 in
    let p2 = node_position_map M.! n2 in
    let e1 = p2 - p1 in
    let e2 = p1 - p2 in
    let id1 = id+1 in
    let id2 = id+2 in
    let id3 = id+3 in
    let id4 = id+4 in
    let mn1 = (id1,NodeInfo(nc1,e1)) in
    let mn2 = (id2,NodeInfo(nc1,e2)) in
    let mn3 = (id3,NodeInfo(nc2,e2)) in
    let mn4 = (id4,NodeInfo(nc2,e1)) in
    (mn1:mn2:mn3:mn4:fn,(id1,id4,c):(id3,id2,c):fde,L.foldl' (f MM.insert) toOuter [(n1,mn1),(n2,mn3)],L.foldl' (f MM.insert) toInner [(n1,mn2),(n2,mn4)],L.foldl' (f M.insert) toParent [(id1,n1),(id2,n1),(id3,n2),(id4,n2)],id+4)
    where 
        f func mm (k,a) = func k a mm

calcAngle :: Vec -> Vec -> Float
calcAngle a b = let ret = acos (dot a b / ((norm a) * (norm b))) in if isNaN ret then 1.0 else ret

addMiniEdges :: FloorDirectedEdges -> MultiMap Node (LNode NodeInfo) -> MultiMap Node (LNode NodeInfo) -> FloorDirectedEdges
addMiniEdges fde toOuter toInner = L.foldl' f fde graph_nodes
    where
        f cur (n,info) = 
            let outer = MM.lookup n toOuter in
            let inner = MM.lookup n toInner in
            L.foldl' (g inner) cur outer
        g inner cur (on,NodeInfo(obc,ov)) =
            L.foldl' (\cur_ -> \(inn,NodeInfo(ibc,iv)) -> let c = calcAngleCost (calcAngle iv ov) in (inn,on,c):cur_) cur inner
            
addParentEdges :: FloorDirectedEdges -> MultiMap Node (LNode NodeInfo) -> MultiMap Node (LNode NodeInfo) -> StartPoint -> EndPoint -> FloorDirectedEdges
addParentEdges fde toOuter toInner (StartPoint s) (EndPoint e) =
    let outer = MM.lookup s toOuter in
    let inner = MM.lookup e toInner in
    L.foldl' (\cur -> \(inn,_) -> (inn,e,Cost 0.0):cur) (L.foldl' (\cur -> \(on,_) -> (s,on,Cost 0.0):cur) fde outer) inner

refinePath :: Map Node Node -> Path -> Path
refinePath toParentNode p = L.map head $ group $ L.map (\n -> fromMaybe n (M.lookup n toParentNode)) p

createGraph :: FloorNodes -> FloorDirectedEdges -> BlockGraph
createGraph nodes directedEdges =
--    let directedEdges = convertDirectedEdges unDirectedEdges in
    mkGraph nodes directedEdges

cuttingEdge :: FloorUnDirectedEdges -> PointsOfBlock -> FloorUnDirectedEdges
cuttingEdge ude poe =
    let tEdges = L.filter (\(l,r,_) -> not((S.member l poe) || (S.member r poe))) ude in
    let middle_edges = S.foldl' (\cur -> \p -> let s = L.foldl' (\c -> \(l,r,_) -> if l==p then S.insert r c else if r==p then S.insert l c else c) S.empty ude in S.union cur (S.fromList (L.filter (\(l,r,_) -> (S.member l s)&&(S.member r s)) graph_middle_edges))) S.empty poe in
    tEdges ++ (S.toList middle_edges)

searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> Maybe (Path, Cost)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    case (sp startPoint endPoint g, spLength startPoint endPoint g) of
        (Just path, Just cost) -> Just (path, cost)
        (_, _) -> Nothing

goto :: [Node] -> StartPoint -> EndPoint -> Maybe (Path,Cost)
goto poe startPoint endPoint = 
    let noblock_ude = cuttingEdge graph_edges (S.fromList poe) in
    let (nodesWithMiniNodes,edgesHavingMiniNodes,nodeToOuterMiniNode,nodeToInnerMiniNode,miniNodeToParentNode,sizeOfNodes) = L.foldl' createRotateBaseEdges (graph_nodes,[],MM.empty,MM.empty,M.empty,length graph_nodes) noblock_ude in
    let edgesWithMiniEdges = addMiniEdges edgesHavingMiniNodes nodeToOuterMiniNode nodeToInnerMiniNode in
    let edgesWithParentEdges = addParentEdges edgesWithMiniEdges nodeToOuterMiniNode nodeToInnerMiniNode startPoint endPoint in
    let g = createGraph nodesWithMiniNodes edgesWithParentEdges in
    maybe Nothing (\(p,c) -> Just (refinePath miniNodeToParentNode p, c)) (searchShortPath startPoint endPoint g)

getColorNode :: FloorNodes -> BlockColor -> Path
getColorNode fn Black = L.map (\(e,_) -> e) fn
getColorNode fn color = 
    L.foldl' (\cur -> \(e,NodeInfo (c,_)) -> if c == color then e:cur else cur) [] fn

isDeadLock :: Path -> [(BlockColor, Node)] -> BlockPosition -> Node -> Bool
isDeadLock checked cl bp e = if L.elem e checked then True
    else let nodes = L.filter (\n -> snd n == e) (A.assocs bp) in 
        if L.null nodes then False
        else let target = head nodes in
            let next = L.find (\(c,n) -> fst target == c) cl in
            maybe False (\(_,n) -> isDeadLock (e:checked) cl bp n) next

findEscapeNode :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> Maybe (Path, Cost)
findEscapeNode bp cl bc s = let onboardnodes = A.elems bp in 
    let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
    let targetnodes = L.map snd cl in
    let checknodes = onboardnodes ++ targetnodes in
    let nodes = L.filter (\n -> not (L.elem n checknodes)) [1..15] in
    let ms = catMaybes $ L.map (\e -> goto poe (StartPoint s) (EndPoint e)) nodes in
    if L.null ms then Nothing else Just (head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms)


searchNextTarget :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchNextTarget bp cl bc e backNode endPoint = solveTarget (bp // [(bc,e)]) cl (StartPoint backNode) endPoint

searchReturnRoot :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Path -> Cost -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchReturnRoot bp cl bc departRoot departCost s e endPoint = 
    let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
    case goto poe (StartPoint s) (EndPoint e) of
        Nothing -> []
        Just (returnRoot, returnCost) ->
            let moveRoot = departRoot ++ (tail returnRoot) in
            let moveCost = departCost + returnCost in
            let backNode = last (init moveRoot) in
            let currentRoot = moveRoot ++ [backNode] in
            let currentCost = moveCost + (edge_cost_map M.! (e,backNode)) in
            let nextTargets = searchNextTarget bp cl bc e backNode endPoint in
            B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) nextTargets

processReturnBlockTarget :: BlockPosition -> [(BlockColor,Node)] -> BlockColor -> Node -> Path -> Cost -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
processReturnBlockTarget bp cl bc bcn departRoot departCost current_block_point endPoint =
    searchRoundRoot departRoot departCost current_block_point bcn
    where
        searchRoundRoot :: Path -> Cost -> Node -> Node -> [(Path,Cost,BlockPosition)]
        searchRoundRoot departRoot departCost s e = let currentCl = (bc,e):cl in
            if isDeadLock [] currentCl bp e
            then case findEscapeNode bp currentCl bc s of
                Nothing -> []
                Just (returnRoot, returnCost) ->
                    let moveRoot = departRoot ++ (tail returnRoot) in
                    let moveCost = departCost + returnCost in
                    let escapeNode = last moveRoot in
                    let backNode = last (init moveRoot) in
                    let currentRoot = moveRoot ++ [backNode] in
                    let currentCost = moveCost + (edge_cost_map M.! (e,backNode)) in
                    let restSolve = solveTarget (bp // [(bc,escapeNode)]) ((bc,bcn):cl) (StartPoint backNode) endPoint in
                    B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) restSolve
            else searchReturnRoot bp cl bc departRoot departCost s e endPoint

processBlockTarget :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
processBlockTarget bp cl bc bcn startPoint endPoint = 
    let current_block_point = bp A.! bc in
    if current_block_point == bcn then solveTarget bp cl startPoint endPoint
    else
        let departEndPoint = EndPoint current_block_point in
        let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
        case goto poe startPoint departEndPoint of
            Nothing -> []
            Just (departRoot, departCost) ->
                processReturnBlockTarget bp cl bc bcn departRoot departCost current_block_point endPoint
            
solveTarget :: BlockPosition -> [(BlockColor, Node)] -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
solveTarget bp [] startPoint endPoint = maybe [] (\(p,c) -> [(p,c,bp)]) (goto (A.elems bp) startPoint endPoint)
solveTarget bp unprocessBlocks startPoint endPoint = 
    concatMap (\p@(color,node) -> processBlockTarget bp (L.delete p unprocessBlocks) color node startPoint endPoint) unprocessBlocks

calcOptimizedRootTarget :: BlockPosition -> [BlockPosition] -> StartPoint -> EndPoint -> Maybe (Path,Cost,BlockPosition)
calcOptimizedRootTarget bp tblist startPoint endPoint = 
    let ans = L.concatMap f tblist in
    if L.null ans then Nothing else Just (minimumBy (\(_,l,_) -> \(_,r,_) -> compare l r) ans)
    where
        f :: BlockPosition -> [(Path,Cost,BlockPosition)]
        f tb = let bplist = A.assocs tb in solveTarget bp bplist startPoint endPoint
        
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

isTargetFigure :: BlockPosition -> Bool
--isTargetFigure a = (isDepressionSquareBonus a) || (isSquareBonus a) || (isPentagonBonus a)
isTargetFigure a = isPentagonBonus a

targetList :: BlockPosition -> [BlockPosition]
targetList bp = let l = L.filter isTargetFigure $ L.filter (not.isCenterBlockBonus) blockArrayRaw in
    let replaceList = L.map (\n -> if not (isPentagonBonus n) then n // [(Black,bp A.! Black)] else n) l in
    let blockPlaceFilterList = L.filter (\n -> let ns = A.elems n in S.size (S.fromList ns) == 5) replaceList in
    S.toList $ S.fromList blockPlaceFilterList

calcTargetRoot :: StartPoint -> EndPoint -> BlockPosition -> [(BonusPoint,Cost,Path,BlockPosition)]
calcTargetRoot sp ep bp =
    concatMap (\tlist -> let bplist = A.assocs tlist in L.map (\(a,b,c) -> (calcBonusPoint graph_nodes tlist,b,a,c)) (solveTarget bp bplist sp ep)) (targetList bp)

createRootFromCode :: Node -> Cost -> InitCode -> [Word8]
createRootFromCode gp cost code = 
    let bp = fromInitCode gp code in 
    if isInitBlockPosition bp 
    then g cost (calcTargetRoot (StartPoint 10) (EndPoint 11) bp) 
    else []
        where
            g :: Cost -> [(BonusPoint,Cost,Path,BlockPosition)] -> [Word8]
            g cost xs = 
                let fs = L.filter (\(_,c,_,_) -> c <= cost) xs in 
                if L.null fs then [] else let (_,_,r,_) = head (sort fs) in L.map fromIntegral r
        
createBinary :: Node -> Cost -> BinaryData
createBinary greenPos maxCost = 
    let bps = L.map (\n -> (n,createRootFromCode greenPos maxCost n)) allInitCode in
    BinaryData $ array (minBound, maxBound) bps

