module BinaryDataSpec (spec) where

import Test.Hspec    
import BinaryData

import Data.Array
import Data.Binary
import Data.ByteString.Lazy

spec :: Spec
spec = do
    describe "put" $ do
        it "sample01" $ do
            let arr = BinaryData $ array (0,15*11*11*11-1) [(i,[]) | i <- [0..15*11*11*11-1]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr

        it "sample02" $ do
            let arr = BinaryData $ array (0,15*11*11*11-1) [(i,Prelude.map fromInteger [1..255]) | i <- [0..15*11*11*11-1]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr            