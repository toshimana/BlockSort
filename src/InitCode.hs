module InitCode where

import Data.Array

newtype InitCode = InitCode Int deriving (Ix,Ord,Eq,Show)