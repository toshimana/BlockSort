module BinaryData where

import Data.Word
import Data.Binary
import Data.Array
import Control.Monad

import InitCode

data BinaryData = BinaryData (Array InitCode [Word8]) deriving (Show,Eq)

instance Binary BinaryData where
    put (BinaryData arr) = do 
        let es = elems arr
        let f w _ = putWord8 w
        let g e = zipWithM_ f (e++(repeat 0)) [1..256]
        mapM_ g es

    get = do
        ms <- mapM (\idx -> mapM (\_ -> getWord8) [1..256] >>= return.(\n->(InitCode idx,takeWhile (/= 0) n))) [0..15*11*11*11-1] 
        return $ createBinaryData ms

createBinaryData :: [(InitCode,[Word8])] -> BinaryData
createBinaryData xs = BinaryData $ array (minBound, maxBound) xs
