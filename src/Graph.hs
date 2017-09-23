module Graph where

import Data.List as L
import Data.Array as A
import Data.Map as M
import Data.Maybe
import Data.Graph.Inductive.Graph
import Data.Graph.Inductive.Tree
import Linear.Metric

import Cost
import GraphConstants

newtype StartPoint = StartPoint Node
newtype EndPoint = EndPoint Node

type BlockGraph = Gr NodeInfo Cost

newtype ParentToOuter = ParentToOuter (Array Node [LNode NodeInfo])
newtype ParentToInner = ParentToInner (Array Node [LNode NodeInfo])

getOuter :: ParentToOuter -> Node -> [LNode NodeInfo]
getOuter (ParentToOuter outer) n = outer A.! n

getInner :: ParentToInner -> Node -> [LNode NodeInfo]
getInner (ParentToInner inner) n = inner A.! n

newtype ChildToParent = ChildToParent (Map Node Node) deriving (Show,Eq)

toParent :: ChildToParent -> Node -> Node
toParent (ChildToParent toParent) n = fromMaybe n (M.lookup n toParent)

newtype InnerToOuter = InnerToOuter (Map Node Node) deriving (Show,Eq)

findOuterFromInner :: InnerToOuter -> Node -> Node
findOuterFromInner (InnerToOuter ito) n = ito M.! n

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

createRotateBaseEdges :: FloorNodes -> FloorUnDirectedEdges -> (FloorNodes,FloorDirectedEdges,ParentToOuter,ParentToInner,ChildToParent,InnerToOuter,Node)
createRotateBaseEdges nodes ude = 
    let (fn,fde,toOuterList,toInnerList,toParentList,innerToOuter,id) = L.foldl' createRotateBaseEdges_ (nodes,[],[],[],[],[],length nodes) ude in
    (fn,fde,ParentToOuter (createMapArray id toOuterList),ParentToInner (createMapArray id toInnerList), ChildToParent (M.fromList toParentList), InnerToOuter (M.fromList innerToOuter), id)

createRotateBaseEdges_ :: (FloorNodes,FloorDirectedEdges,[(Node,LNode NodeInfo)],[(Node,LNode NodeInfo)],[(Node,Node)],[(Node,Node)],Node) -> LEdge Cost -> (FloorNodes,FloorDirectedEdges,[(Node,LNode NodeInfo)],[(Node,LNode NodeInfo)],[(Node,Node)],[(Node,Node)],Node)
createRotateBaseEdges_ (fn,fde,toOuter,toInner,toParent,innerToOuter,id) e@(n1,n2,c) = 
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
    (mn1:mn2:mn3:mn4:fn,(id1,id4,c):(id3,id2,c):fde,(n1,mn1):(n2,mn3):toOuter,(n1,mn2):(n2,mn4):toInner,(id1,n1):(id2,n1):(id3,n2):(id4,n2):toParent,(id4,id3):(id2,id1):innerToOuter,id+4)

(nodesWithMiniNodes,edgesHavingMiniNodes,nodeToOuterMiniNode,nodeToInnerMiniNode,miniNodeToParentNode,innerToOuter, sizeOfNodes) = createRotateBaseEdges graph_nodes graph_edges
