module GraphConstants (FloorUnDirectedEdges(..), node_color_map, graph_nodes, graph_edges_with_center, graph_edges) where

import Data.List as L
import Data.Map as M
import Data.Graph.Inductive.Graph

import BlockColor

newtype FloorUnDirectedEdges = FloorUnDirectedEdges [LEdge Float]

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
        