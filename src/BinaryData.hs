module BinaryData where

import Data.Word
import Data.Binary
import Data.Array
import Control.Monad

data BinaryData = BinaryData (Array Int [Word8]) deriving (Show,Eq)

instance Binary BinaryData where
    put (BinaryData arr) = do 
        let es = elems arr
        let f w _ = putWord8 w
        let g e = zipWithM_ f (e++(repeat 0)) [1..256]
        mapM_ g es

    get = do
        ms <- mapM (\idx -> mapM (\_ -> getWord8) [1..256] >>= return.(\n->(idx,takeWhile (/= 0) n))) [1..15*11*11*11] 
        return $ BinaryData (array (1,15*11*11*11) ms)