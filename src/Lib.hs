module Lib (StartPoint(..), EndPoint(..), processBlockTarget, addMiniEdges, addParentEdges, goto, createGraph, searchShortPath, solveTarget, calcOptimizedRootTarget, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode, targetList) where

import Data.Array as A
import Data.Map as M
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
import Cost

type BlockGraph = Gr NodeInfo Cost

type PointsOfBlock = Set Node

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

calcAngle :: Vec -> Vec -> Angle
calcAngle a b = let ret = acos (dot a b / ((norm a) * (norm b))) in Angle (if isNaN ret then 1.0 else ret)

addMiniEdges :: [Node] -> (Angle -> Cost) -> ParentToOuter -> ParentToInner -> FloorDirectedEdges
addMiniEdges nodes costFunc toOuter toInner = L.foldl' f [] nodes
    where
        f cur n = 
            let outer = getOuter toOuter n in
            let inner = getInner toInner n in
            L.foldl' (g inner) cur outer
        g inner cur (on,NodeInfo(obc,ov)) =
            L.foldl' (\cur_ -> \(inn,NodeInfo(ibc,iv)) -> let c = costFunc (calcAngle iv ov) in (inn,on,c):cur_) cur inner
            
addParentEdges :: ParentToOuter -> ParentToInner -> StartPoint -> EndPoint -> FloorDirectedEdges
addParentEdges toOuter toInner (StartPoint s) (EndPoint e) =
    let outer = getOuter toOuter s in
    let inner = getInner toInner e in
    L.foldl' (\cur -> \(inn,_) -> (inn,e,Cost 0.0):cur) (L.foldl' (\cur -> \(on,_) -> (s,on,Cost 0.0):cur) [] outer) inner

refinePath :: ChildToParent -> Path -> Path
refinePath childToParent p = L.map head $ group $ L.map (toParent childToParent) p

createGraph :: FloorNodes -> FloorDirectedEdges -> BlockGraph
createGraph nodes directedEdges = mkGraph nodes directedEdges

searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> Maybe (Path, Cost)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    case (sp startPoint endPoint g, spLength startPoint endPoint g) of
        (Just path, Just cost) -> Just (path, cost)
        (_, _) -> Nothing

goto :: [Node] -> (Angle -> Cost) -> StartPoint -> EndPoint -> Maybe (Path,Cost)
goto poe costFunc startPoint endPoint = 
    let miniEdges = addMiniEdges (L.filter (\n -> L.notElem n poe) [1..18])costFunc nodeToOuterMiniNode nodeToInnerMiniNode in
    let parentEdges = addParentEdges nodeToOuterMiniNode nodeToInnerMiniNode startPoint endPoint in
    let g = createGraph nodesWithMiniNodes (parentEdges++miniEdges++edgesHavingMiniNodes) in
    searchShortPath startPoint endPoint g

getColorNode :: FloorNodes -> BlockColor -> Path
getColorNode fn Black = L.map (\(e,_) -> e) fn
getColorNode fn color = 
    L.foldl' (\cur -> \(e,NodeInfo (c,_)) -> if c == color then e:cur else cur) [] fn

isMovableDestination :: BlockPosition -> Node -> Bool
isMovableDestination bp e = not (L.elem e (A.elems bp))

isDeadLock :: [Node] -> BlockPosition -> [(BlockColor,Node)] -> Node -> Bool
isDeadLock checked bp cl e = 
    if L.elem e checked then True
    else maybe False f (L.find (\n -> snd n == e) (A.assocs bp)) 
    where
        f target = let next = L.find (\(c,n) -> fst target == c) cl in
            maybe False (\(_,n) -> isDeadLock (e:checked) bp cl n) next

findEscapeNode :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Path -> Cost -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
findEscapeNode bp cl tlist bc departRoot departCost s e endPoint =
    concatMap f tlist
    where
        f :: BlockPosition -> [(Path,Cost,BlockPosition)]
        f target =
            let unproceddBlocks = L.map (\c -> (c,target A.! c)) (bc:cl) in
            if isDeadLock [] bp unproceddBlocks e then
                let targetNodes = A.elems target in
                let nodes = L.filter (\n -> not (L.elem n targetNodes)) [1..15] in
                let poe = L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp)) in
                let ms = catMaybes $ L.map (\t -> goto poe calcReturnCostFromAngle (StartPoint s) (EndPoint t)) nodes in
                if L.null ms then [] 
                else let (returnRoot, returnCost) = head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms in
                    let moveRoot = departRoot ++ (tail returnRoot) in
                    let moveCost = departCost + returnCost in
                    let escapeNode = last moveRoot in
                    let currentRoot = init moveRoot in
                    let backNode = last currentRoot in
                    let turnNode = findOuterFromInner innerToOuter backNode in
                    let currentCost = moveCost + (calcDepartCostFromAngle (Angle pi)) in
                    let restSolve = solveTarget (bp // [(bc,escapeNode)]) (bc:cl) [target] (StartPoint turnNode) endPoint in
                    B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ path, currentCost + cost, newbp)) restSolve
            else []
            
searchNextTarget :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchNextTarget bp cl tlist bc e backNode endPoint = solveTarget (bp // [(bc,e)]) cl tlist (StartPoint backNode) endPoint

searchReturnRoot :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Path -> Cost -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchReturnRoot bp cl tlist bc departRoot departCost s e endPoint = 
    let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
    case goto poe calcReturnCostFromAngle (StartPoint s) (EndPoint e) of
        Nothing -> []
        Just (returnRoot, returnCost) ->
            let moveRoot = departRoot ++ (tail returnRoot) in
            let moveCost = departCost + returnCost in
            let currentRoot = init moveRoot in
            let backNode = last currentRoot in
            let turnNode = findOuterFromInner innerToOuter backNode in
            let currentCost = moveCost + (calcDepartCostFromAngle (Angle pi)) in
            let nextTargets = searchNextTarget bp cl tlist bc e turnNode endPoint in
            B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ path, currentCost + cost, newbp)) nextTargets

processReturnBlockTarget :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Node -> Path -> Cost -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
processReturnBlockTarget bp cl tlist bc bcn departRoot departCost current_block_point endPoint =
    searchRoundRoot departRoot departCost current_block_point bcn
    where
        searchRoundRoot :: Path -> Cost -> Node -> Node -> [(Path,Cost,BlockPosition)]
        searchRoundRoot departRoot departCost s e = let currentCl = bc:cl in
            if isMovableDestination bp e
            then searchReturnRoot bp cl tlist bc departRoot departCost s e endPoint
            else findEscapeNode bp cl tlist bc departRoot departCost s e endPoint

processBlockTarget :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
processBlockTarget bp cl tlist bc bcn startPoint endPoint = 
    let current_block_point = bp A.! bc in
    if current_block_point == bcn then solveTarget bp cl tlist startPoint endPoint
    else
        let departEndPoint = EndPoint current_block_point in
        let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
        case goto poe calcDepartCostFromAngle startPoint departEndPoint of
            Nothing -> []
            Just (departRoot, departCost) ->
                let departRootRemoveLast = init departRoot in
                processReturnBlockTarget bp cl tlist bc bcn departRootRemoveLast departCost (last departRootRemoveLast) endPoint
            
solveTarget :: BlockPosition -> [BlockColor] -> [BlockPosition] -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
solveTarget bp [] tlist startPoint endPoint = maybe [] (\(p,c) -> [(p,c,bp)]) (goto (A.elems bp) calcDepartCostFromAngle startPoint endPoint)
solveTarget bp unprocessBlocks tlist startPoint endPoint = 
    concatMap f unprocessBlocks
    where
        f color = 
            let nodes = S.toList $ S.fromList $ L.map (\t -> t A.! color) tlist in
            let restColors = L.delete color unprocessBlocks in
            concatMap (\n -> let newtlist = L.filter (\t -> (t A.! color) == n) tlist in processBlockTarget bp restColors newtlist color n startPoint endPoint) nodes

calcOptimizedRootTarget :: BlockPosition -> [BlockPosition] -> StartPoint -> EndPoint -> Maybe (Path,Cost,BlockPosition)
calcOptimizedRootTarget bp tblist startPoint endPoint = 
    let ans = solveTarget bp [Red .. Black] tblist startPoint endPoint in
    if L.null ans then Nothing else Just (minimumBy (\(_,l,_) -> \(_,r,_) -> compare l r) ans)
        
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
isTargetFigure a = (not (isCenterBlockBonus a)) && ((isDepressionSquareBonus a) || (isSquareBonus a) || (isPentagonBonus a))


targetList :: BlockPosition -> [BlockPosition]
targetList bp = let l = L.filter isTargetFigure blockArrayRaw in
    let replaceList = L.map (\n -> if not (isPentagonBonus n) then n // [(Black,bp A.! Black)] else n) l in
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
                if L.null fs then [] else let (_,_,r,_) = head (sort fs) in L.map fromIntegral r
        
createBinary :: Node -> Cost -> BinaryData
createBinary greenPos maxCost = 
    let bps = L.map (\n -> (n,createRootFromCode greenPos maxCost n)) allInitCode in
    BinaryData $ array (minBound, maxBound) bps

