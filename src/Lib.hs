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

type BlockGraph = Gr NodeInfo Cost

type PointsOfBlock = Set Node

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges edges =
    concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

calcAngle :: Vec -> Vec -> Float
calcAngle a b = let ret = acos (dot a b / ((norm a) * (norm b))) in if isNaN ret then 1.0 else ret

addMiniEdges :: FloorDirectedEdges -> (Float -> Cost) -> Array Node [LNode NodeInfo] -> Array Node [LNode NodeInfo] -> FloorDirectedEdges
addMiniEdges fde costFunc toOuter toInner = L.foldl' f fde graph_nodes
    where
        f cur (n,info) = 
            let outer = toOuter A.! n in
            let inner = toInner A.! n in
            L.foldl' (g inner) cur outer
        g inner cur (on,NodeInfo(obc,ov)) =
            L.foldl' (\cur_ -> \(inn,NodeInfo(ibc,iv)) -> let c = costFunc (calcAngle iv ov) in (inn,on,c):cur_) cur inner
            
addParentEdges :: FloorDirectedEdges -> Array Node [LNode NodeInfo] -> Array Node [LNode NodeInfo] -> StartPoint -> EndPoint -> FloorDirectedEdges
addParentEdges fde toOuter toInner (StartPoint s) (EndPoint e) =
    let outer = toOuter A.! s in
    let inner = toInner A.! e in
    L.foldl' (\cur -> \(inn,_) -> (inn,e,Cost 0.0):cur) (L.foldl' (\cur -> \(on,_) -> (s,on,Cost 0.0):cur) fde outer) inner

refinePath :: Map Node Node -> Path -> Path
refinePath toParentNode p = L.map head $ group $ L.map (\n -> fromMaybe n (M.lookup n toParentNode)) p

createGraph :: FloorNodes -> FloorDirectedEdges -> BlockGraph
createGraph nodes directedEdges =
--    let directedEdges = convertDirectedEdges unDirectedEdges in
    mkGraph nodes directedEdges

cuttingEdge :: FloorDirectedEdges -> PointsOfBlock -> FloorDirectedEdges
cuttingEdge de poe = S.foldl' f de poe
    where
        f :: FloorDirectedEdges -> Node -> FloorDirectedEdges
        f cur n = 
            let outer = L.map fst $ nodeToOuterMiniNode A.! n in
            let inner = L.map fst $ nodeToInnerMiniNode A.! n in
            L.filter (\(s,e,_) -> not ((L.elem s inner)&&(L.elem e outer))) cur
    
searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> Maybe (Path, Cost)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    case (sp startPoint endPoint g, spLength startPoint endPoint g) of
        (Just path, Just cost) -> Just (path, cost)
        (_, _) -> Nothing

goto :: [Node] -> (Float -> Cost) -> StartPoint -> EndPoint -> Maybe (Path,Cost)
goto poe costFunc startPoint endPoint = 
    let edgesWithMiniEdges = addMiniEdges edgesHavingMiniNodes costFunc nodeToOuterMiniNode nodeToInnerMiniNode in
    let noblock_ude = cuttingEdge edgesWithMiniEdges (S.fromList poe) in
    let edgesWithParentEdges = addParentEdges noblock_ude nodeToOuterMiniNode nodeToInnerMiniNode startPoint endPoint in
    let g = createGraph nodesWithMiniNodes edgesWithParentEdges in
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

findEscapeNode :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Node -> [(Path,Cost,BlockPosition)]
findEscapeNode bp cl tlist bc e =
    concatMap f tlist
    where
        f :: BlockPosition -> [(Path,Cost,BlockPosition)]
        f target = 
            let unproceddBlocks = L.map (\c -> (c,target A.! c)) cl in
            if isDeadLock [] bp unproceddBlocks e then
                let targetNodes = A.elems target in
                let nodes = L.filter (\n -> not (L.elem n targetNodes)) [1..15] in
                undefined
            else []
            
--    let onboardnodes = A.elems bp in 
--    let poe = (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
--    let targetnodes = L.map snd cl in
--    let checknodes = onboardnodes ++ targetnodes in
--    let nodes = L.filter (\n -> not (L.elem n checknodes)) [1..15] in
--    let ms = catMaybes $ L.map (\e -> goto poe calcReturnCostFromAngle (StartPoint s) (EndPoint e)) nodes in
--    if L.null ms then Nothing else Just (head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms)


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
            let turnNode = innerToOuter M.! backNode in
            let currentCost = moveCost + (calcDepartCostFromAngle pi) in
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
            else findEscapeNode bp cl tlist bc e
--                Nothing -> []
--                Just (returnRoot, returnCost) ->
--                    let moveRoot = departRoot ++ (tail returnRoot) in
--                    let moveCost = departCost + returnCost in
--                    let escapeNode = last moveRoot in
--                    let currentRoot = init moveRoot in
--                    let backNode = last currentRoot in
--                    let turnNode = innerToOuter M.! backNode in
--                    let currentCost = moveCost + (calcDepartCostFromAngle pi) in
--                    let restSolve = solveTarget (bp // [(bc,escapeNode)]) (bc:cl) tlist (StartPoint turnNode) endPoint in
--                    B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) restSolve

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
    concatMap (\p@color -> processBlockTarget bp (L.delete p unprocessBlocks) tlist color 0 startPoint endPoint) unprocessBlocks

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
--isTargetFigure a = isPentagonBonus a

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

