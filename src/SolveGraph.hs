module SolveGraph where

import qualified Data.List as L
import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Maybe as B

import BlockColor
import BonusPoint
import Graph
import Cost
import Data.Graph.Inductive.Graph
import GraphConstants

isMovableDestination :: BlockPosition -> Node -> Bool
isMovableDestination bp e = not (L.elem e (A.elems bp))

isDeadLock :: [Node] -> BlockPosition -> [(BlockColor,Node)] -> Node -> Bool
isDeadLock checked bp cl e = 
    if L.elem e checked then True
    else maybe False f (L.find (\n -> snd n == e) (A.assocs bp)) 
    where
        f target = let next = L.find (\(c,n) -> fst target == c) cl in
            maybe False (\(_,n) -> isDeadLock (e:checked) bp cl n) next

goto :: [Node] -> (Angle -> Cost) -> StartPoint -> EndPoint -> Maybe (Path,Cost)
goto poe costFunc startPoint endPoint = 
    let miniEdges = addMiniEdges (L.filter (\n -> L.notElem n poe) [1..18])costFunc nodeToOuterMiniNode nodeToInnerMiniNode in
    let parentEdges = addParentEdges nodeToOuterMiniNode nodeToInnerMiniNode startPoint endPoint in
    let g = createGraph nodesWithMiniNodes (parentEdges++miniEdges++edgesHavingMiniNodes) in
    searchShortPath startPoint endPoint g

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
                let ms = B.catMaybes $ L.map (\t -> goto poe calcReturnCostFromAngle (StartPoint s) (EndPoint t)) nodes in
                if L.null ms then [] 
                else let (returnRoot, returnCost) = head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms in
                    let moveRoot = departRoot ++ (tail returnRoot) in
                    let moveCost = departCost + returnCost in
                    let escapeNode = last moveRoot in
                    let currentRoot = init moveRoot in
                    let backNode = last currentRoot in
                    let turnNode = findOuterFromInner innerToOuter backNode in
                    let currentCost = moveCost + (calcDepartCostFromAngle (Angle pi)) in
                    let restSolve = solveTarget (bp A.// [(bc,escapeNode)]) (bc:cl) [target] (StartPoint turnNode) endPoint in
                    B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ path, currentCost + cost, newbp)) restSolve
            else []

searchNextTarget :: BlockPosition -> [BlockColor] -> [BlockPosition] -> BlockColor -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchNextTarget bp cl tlist bc e backNode endPoint = solveTarget (bp A.// [(bc,e)]) cl tlist (StartPoint backNode) endPoint

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

calcTargetRoot :: StartPoint -> EndPoint -> BlockPosition -> [BlockPosition] -> [(BonusPoint,Cost,Path,BlockPosition)]
calcTargetRoot sp ep bp tlist = 
    let ans = solveTarget bp [Red .. Black] tlist sp ep in
    L.map (\(p,c,bp) -> (calcBonusPoint graph_nodes bp,c,init (tail (refinePath miniNodeToParentNode p)),bp)) ans
