module FloorNodes where

import Data.Graph.Inductive.Graph

import BlockColor

data FloorNodes = FloorNodes [LNode BlockColor]
