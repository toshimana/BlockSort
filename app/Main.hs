module Main where

import Lib
import Data.Graph.Inductive.PatriciaTree
import Data.Graph.Inductive.Graph 
import Data.Graph.Inductive.Query.SP

graph_nodes :: [LNode Int]
graph_nodes = [(i,1) | i <- [1..15]]

graph_edges_ :: [LEdge Int]
graph_edges_ = [(1,2,1),(1,5,1),(1,10,1),(2,1,1),(2,3,1),(2,5,1),(2,6,1),(3,4,1),(3,6,1),(3,7,1),(4,7,1),(4,11,1),(5,8,1),(5,10,1),(6,8,1),(6,9,1),(7,9,1),(7,11,1),(10,12,1),(11,15,1),(12,13,1),(13,14,1),(14,15,1)]

graph_edges :: [LEdge Int]
graph_edges = concat [[(i,j,k),(j,i,k)] | (i,j,k)<-graph_edges_]

g :: Gr Int Int
g = mkGraph graph_nodes graph_edges

main :: IO ()
main = do
    print (sp 10 11 g)
    someFunc
