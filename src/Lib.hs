module Lib (StartPoint(..), EndPoint(..), Cost(..), processBlockTarget, solveTarget, calcOptimizedRootTarget, answerList, calcTargetRoot, createBinary, blockArray, isDeadLock, createRootFromCode, targetList) where

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

type BlockGraph = Gr NodeInfo Cost

data PointsOfBlock = PointsOfBlock (Set Node)
data FloorDirectedEdges = FloorDirectedEdges [LEdge Float]

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

newtype Cost = Cost Float deriving (Ord,Eq,Show)

instance Num Cost where
    (+) (Cost a) (Cost b) = Cost (a+b)
    (-) (Cost a) (Cost b) = Cost (a-b)
    (*) (Cost a) (Cost b) = Cost (a*b)
    negate (Cost a) = Cost (negate a)
    abs (Cost a) = Cost (abs a)
    signum (Cost a) = Cost (signum a)
    fromInteger a = Cost (fromInteger a)

instance Real Cost where
    toRational (Cost a) = toRational a

append_graph_edges :: FloorUnDirectedEdges -> FloorUnDirectedEdges -> FloorUnDirectedEdges
append_graph_edges (FloorUnDirectedEdges l) (FloorUnDirectedEdges r) = FloorUnDirectedEdges (l++r)

convertDirectedEdges :: FloorUnDirectedEdges -> FloorDirectedEdges
convertDirectedEdges (FloorUnDirectedEdges edges) =
    FloorDirectedEdges $ concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createGraph :: FloorNodes -> FloorUnDirectedEdges -> BlockGraph
createGraph (FloorNodes nodes) unDirectedEdges = 
    let (FloorDirectedEdges directedEdges) = convertDirectedEdges unDirectedEdges in
    mkGraph nodes (L.map (\(a,b,c) -> (a,b,Cost c)) directedEdges)

cuttingEdge :: FloorUnDirectedEdges -> PointsOfBlock -> FloorUnDirectedEdges
cuttingEdge (FloorUnDirectedEdges ude) (PointsOfBlock poe) =
    let tEdges = L.filter (\(l,r,_) -> not((S.member l poe) || (S.member r poe))) ude in
    let FloorUnDirectedEdges gme = graph_middle_edges in
    let middle_edges = S.foldl' (\cur -> \p -> let s = L.foldl' (\c -> \(l,r,_) -> if l==p then S.insert r c else if r==p then S.insert l c else c) S.empty ude in S.union cur (S.fromList (L.filter (\(l,r,_) -> (S.member l s)&&(S.member r s)) gme))) S.empty poe in
    FloorUnDirectedEdges (tEdges ++ (S.toList middle_edges))

searchShortPath :: StartPoint -> EndPoint -> BlockGraph -> Maybe (Path, Cost)
searchShortPath (StartPoint startPoint) (EndPoint endPoint) g =
    case (sp startPoint endPoint g, spLength startPoint endPoint g) of
        (Just path, Just cost) -> Just (path, cost)
        (_, _) -> Nothing

gotoend :: BlockPosition -> StartPoint -> EndPoint -> Maybe (Path,Cost,BlockPosition)
gotoend bp startPoint endPoint = 
    let poe = PointsOfBlock $ S.fromList (A.elems bp) in
    let noblock_ude = cuttingEdge graph_edges poe in
    let g = createGraph graph_nodes noblock_ude in
    case searchShortPath startPoint endPoint g of
        Just (path, cost) -> Just (path, cost, bp)
        Nothing -> Nothing

getColorNode :: FloorNodes -> BlockColor -> Path
getColorNode (FloorNodes fn) Black = L.map (\(e,_) -> e) fn
getColorNode (FloorNodes fn) color = 
    L.foldl' (\cur -> \(e,NodeInfo (c,_)) -> if c == color then e:cur else cur) [] fn

isDeadLock :: Path -> [(BlockColor, Node)] -> BlockPosition -> Node -> Bool
isDeadLock checked cl bp e = if L.elem e checked then True
    else let nodes = L.filter (\n -> snd n == e) (A.assocs bp) in 
        if L.null nodes then False
        else let target = head nodes in
            let next = L.find (\(c,n) -> fst target == c) cl in
            maybe False (\(_,n) -> isDeadLock (e:checked) cl bp n) next

findEscapeNode :: BlockGraph -> BlockPosition -> [(BlockColor, Node)] -> Node -> Maybe (Path, Cost)
findEscapeNode g bp cl s = let onboardnodes = A.elems bp in 
    let targetnodes = L.map snd cl in
    let checknodes = onboardnodes ++ targetnodes in
    let nodes = L.filter (\n -> not (L.elem n checknodes)) [1..15] in
    let ms = catMaybes $ L.map (\e -> searchShortPath (StartPoint s) (EndPoint e) g) nodes in
    if L.null ms then Nothing else Just (head $ L.sortBy (\(_,l) -> \(_,r) -> compare l r) ms)


searchNextTarget :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchNextTarget bp cl bc e backNode endPoint = solveTarget (bp // [(bc,e)]) cl (StartPoint backNode) endPoint

searchReturnRoot :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> BlockGraph -> Path -> Cost -> Node -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
searchReturnRoot bp cl bc g departRoot departCost s e endPoint = 
    case searchShortPath (StartPoint s) (EndPoint e) g of
        Nothing -> []
        Just (returnRoot, returnCost) ->
            let moveRoot = departRoot ++ (tail returnRoot) in
            let moveCost = departCost + returnCost in
            let backNode = last (init moveRoot) in
            let currentRoot = moveRoot ++ [backNode] in
            let currentCost = maybe 0 ((+) moveCost) (spLength e backNode g) in
            let nextTargets = searchNextTarget bp cl bc e backNode endPoint in
            B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) nextTargets

processReturnBlockTarget :: BlockPosition -> [(BlockColor,Node)] -> BlockColor -> Node -> BlockGraph -> Path -> Cost -> Node -> EndPoint -> [(Path,Cost,BlockPosition)]
processReturnBlockTarget bp cl bc bcn g departRoot departCost current_block_point endPoint =
    if (bc,bcn) == (Black,16) then 
        let poe = PointsOfBlock $ S.fromList (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
        searchCenter poe departRoot departCost current_block_point 
    else searchRoundRoot g departRoot departCost current_block_point bcn
    where
        searchRoundRoot :: BlockGraph -> Path -> Cost -> Node -> Node -> [(Path,Cost,BlockPosition)]
        searchRoundRoot g departRoot departCost s e = let currentCl = (bc,e):cl in
            if isDeadLock [] currentCl bp e
            then case findEscapeNode g bp currentCl s of
                Nothing -> []
                Just (returnRoot, returnCost) ->
                    let moveRoot = departRoot ++ (tail returnRoot) in
                    let moveCost = departCost + returnCost in
                    let escapeNode = last moveRoot in
                    let backNode = last (init moveRoot) in
                    let currentRoot = moveRoot ++ [backNode] in
                    let currentCost = maybe 0 ((+) moveCost) (spLength e backNode g) in
                    let restSolve = solveTarget (bp // [(bc,escapeNode)]) ((bc,bcn):cl) (StartPoint backNode) endPoint in
                    B.mapMaybe (\(path,cost,newbp) -> if L.null path then Nothing else Just (currentRoot ++ (tail path), currentCost + cost, newbp)) restSolve
            else searchReturnRoot bp cl bc g departRoot departCost s e endPoint
        searchCenter :: PointsOfBlock -> Path -> Cost -> Node -> [(Path,Cost,BlockPosition)]
        searchCenter poe departRoot departCost s = 
            let noblock_ude = cuttingEdge (append_graph_edges graph_edges graph_edges_with_center) poe in
            let g = createGraph graph_nodes noblock_ude in
            searchReturnRoot bp cl bc g departRoot departCost s 16 endPoint

processBlockTarget :: BlockPosition -> [(BlockColor, Node)] -> BlockColor -> Node -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
processBlockTarget bp cl bc bcn startPoint endPoint = 
    let current_block_point = bp A.! bc in
    if current_block_point == bcn then solveTarget bp cl startPoint endPoint
    else
        let poe = PointsOfBlock $ S.fromList (L.map snd (L.filter (\(c,_) -> c /= bc) (A.assocs bp))) in
        let noblock_ude = cuttingEdge graph_edges poe in
        let g = createGraph graph_nodes noblock_ude in
        let departEndPoint = EndPoint current_block_point in
        case searchShortPath startPoint departEndPoint g of
            Nothing -> []
            Just (departRoot, departCost) ->
                processReturnBlockTarget bp cl bc bcn g departRoot departCost current_block_point endPoint
            
solveTarget :: BlockPosition -> [(BlockColor, Node)] -> StartPoint -> EndPoint -> [(Path,Cost,BlockPosition)]
solveTarget bp [] startPoint endPoint = maybeToList (gotoend bp startPoint endPoint)
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

