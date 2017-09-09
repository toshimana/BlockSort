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
            let arr = BinaryData $ array (InitCode 0,InitCode (15*11*11*11-1)) [(InitCode i,[]) | i <- [0..15*11*11*11-1]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr

        it "sample02" $ do
            let arr = BinaryData $ array (InitCode 0,InitCode (15*11*11*11-1)) [(InitCode i,Prelude.map fromInteger [1..255]) | i <- [0..15*11*11*11-1]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr            