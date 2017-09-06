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
            let arr = BinaryData $ array (1,16*11*11*11) [(i,[]) | i <- [1..16*11*11*11]]
            let encArr = encode arr
            let decArr = decode encArr :: BinaryData
            shouldBe decArr arr