module LibSpec (spec) where

import Test.Hspec
import Data.Array
import Data.List as L
import Data.Map as M
import Data.Maybe
import Linear

import Lib
import BlockColor
import InitCode
import GraphConstants
import BonusPoint
import Cost

spec :: Spec
spec = do
    describe "solve" $ do
        it "sample11" $ do
            let ic = toInitCode $ array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            createRootFromCode 1 4000 ic `shouldBe` [10,12,13,14,13,8,5,2,6,8,6,3,7,9,7,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11]

        it "sample12" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,10)]
            [calcOptimizedRootTarget bns [array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 17) (EndPoint 18)] `shouldBe` [Nothing]

    describe "toInitCode" $ do
        it "sample01" $ do
            toInitCode (array (Red,Black) [(Red,15),(Blue,14),(Yellow,12),(Black,13)])`shouldBe` InitCode ((13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1))
        
        it "sample02" $ do
            let code = InitCode $ (13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1)
            fromInitCode 1 code `shouldBe` (array (Red,Black) [(Red,15),(Green,1),(Blue,14),(Yellow,12),(Black,13)])

        it "sample03" $ do
            let arr = array (Red,Black) [(Red,15),(Green,1),(Blue,14),(Yellow,12),(Black,13)]
            fromInitCode 1 (toInitCode arr) `shouldBe` arr

        it "sample04" $ do
            let code = InitCode $ (13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1)
            toInitCode (fromInitCode 1 code) `shouldBe` code
    
