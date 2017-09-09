module BlockColor(BlockColor(..), FloorNodes(..), BlockPosition) where

import Data.Array
import Data.Graph.Inductive.Graph

data BlockColor = Red | Green | Blue | Yellow | Black | None deriving (Ord, Eq, Ix, Show)
newtype FloorNodes = FloorNodes [LNode BlockColor]

type BlockPosition = Array BlockColor Node
