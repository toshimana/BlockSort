module BinaryDataSpec (spec) where

import Test.Hspec    
import BinaryData

import Data.Array
import Data.Binary
import Data.ByteString.Lazy

import InitCode

spec :: Spec
spec = do
    describe "put" $ do
        it "sample01" $ do
            let arr = BinaryData $ array (minBound, maxBound) [(i,[]) | i <- allInitCode]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr

        it "sample02" $ do
            let arr = BinaryData $ array (minBound, maxBound) [(i,Prelude.map fromInteger [1..255]) | i <- allInitCode]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr            