module Main where

import Lib
import Data.Array
import Data.Map
import Data.List
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Query.SP

data BlockColor = Red | Green | Blue | Yellow | Black deriving (Ord, Eq, Ix)

type BlockPosition = Array BlockColor Node

data BlockNodes = BlockNodes [LNode BlockColor]
data BlockUnDirectedEdges = BlockUnDirectedEdges [LEdge Int]
data BlockDirectedEdges = BlockDirectedEdges [LEdge Int]

node_color_list :: [(Node, BlockColor)]
node_color_list = [(1,Red),(2,Blue),(3,Yellow),(4,Blue),(5,Yellow),(6,Green),(7,Red),(8,Red),(9,Blue),(10,Green),(11,Green),(12,Blue),(13,Yellow),(14,Red),(15,Yellow)]

node_color_map :: Map Node BlockColor
node_color_map = fromList node_color_list

graph_nodes :: BlockNodes
graph_nodes = BlockNodes node_color_list

graph_edges :: BlockUnDirectedEdges
graph_edges = BlockUnDirectedEdges [(1,2,1),(1,5,1),(1,10,1),(2,1,1),(2,3,1),(2,5,1),(2,6,1),(3,4,1),(3,6,1),(3,7,1),(4,7,1),(4,11,1),(5,8,1),(5,10,1),(6,8,1),(6,9,1),(7,9,1),(7,11,1),(10,12,1),(11,15,1),(12,13,1),(13,14,1),(14,15,1)]

createDirectedEdges :: BlockUnDirectedEdges -> BlockDirectedEdges
createDirectedEdges (BlockUnDirectedEdges edges) =
    BlockDirectedEdges $ concat [[(i,j,k),(j,i,k)] | (i,j,k) <- edges]

createGraph :: BlockNodes -> BlockUnDirectedEdges -> Gr BlockColor Int
createGraph (BlockNodes nodes) unDirectedEdges = 
    let (BlockDirectedEdges directedEdges) = createDirectedEdges unDirectedEdges in
    mkGraph nodes directedEdges

calcPolygonBlockBonus :: BlockNodes -> BlockPosition -> Int
calcPolygonBlockBonus bn bp = Data.List.foldl' checkColor 0 colorNodeList
        where
            colorNodeList :: [(BlockColor, Node)]
            colorNodeList = Data.Array.assocs bp
            checkColor :: Int -> (BlockColor, Node) -> Int
            checkColor cur (color, node) = if color == getNodeColor node then cur + 2 else cur
            getNodeColor :: Node -> BlockColor
            getNodeColor n = (Data.Map.!) node_color_map n
            

calcCenterBlockBonus :: BlockNodes -> BlockPosition -> Int
calcCenterBlockBonus bn bp = 0

calcFigureBonus :: BlockNodes -> BlockPosition -> Int
calcFigureBonus bn bp = 0

calcBonusPoint :: BlockNodes -> BlockPosition -> Int
calcBonusPoint bn bp = 
    let polygonBlockBonus = calcPolygonBlockBonus bn bp in
    let centerBlockBonus = calcCenterBlockBonus bn bp in
    let figureBonus = calcFigureBonus bn bp in
    polygonBlockBonus + centerBlockBonus + figureBonus

main :: IO ()
main = do
    let g = createGraph graph_nodes graph_edges
    print (sp 10 11 g)
    let bns = array (Red, Black) [(Red,1),(Green,2),(Blue,3),(Yellow,4),(Black,5)]
    print $ calcBonusPoint graph_nodes bns
    someFunc
