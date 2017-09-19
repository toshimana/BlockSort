module BlockColor(BlockColor(..), BlockPosition) where

import Data.Array
import Data.Graph.Inductive.Graph

data BlockColor = Red | Green | Blue | Yellow | Black | None deriving (Ord, Eq, Ix, Enum, Show)

type BlockPosition = Array BlockColor Node
