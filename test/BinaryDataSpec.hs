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
            let arr = BinaryData $ array (1,15*11*11*11) [(i,[]) | i <- [1..15*11*11*11]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr

        it "sample02" $ do
            let arr = BinaryData $ array (1,15*11*11*11) [(i,Prelude.map fromInteger [1..255]) | i <- [1..15*11*11*11]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr            