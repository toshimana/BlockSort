module LibSpec (spec) where

import Test.Hspec
import Data.Array

import Lib
import BlockColor
import StartPoint
import EndPoint

spec :: Spec
spec = do
    describe "solve" $ do
        it "sample04" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,5)]
            [calcOptimizedRootTarget graph_nodes graph_edges bns [array (Red,Black) [(Red,1),(Green,10),(Blue,2),(Yellow,5),(Black,16)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 10) (EndPoint 11)] `shouldBe` []
            
        it "sample05" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,5)]
            solveTarget graph_nodes graph_edges bns [(Red,1),(Green,10),(Blue,2),(Yellow,5),(Black,16)] (StartPoint 10) (EndPoint 11) `shouldBe` []
            
        it "sample06" $ do
            let bns = array (Red,Black) [(Red,1),(Green,10),(Blue,3),(Yellow,5),(Black,16)]
            processBlockTarget graph_nodes graph_edges bns [] Blue 2 (StartPoint 2) (EndPoint 11) `shouldBe` []
            
        it "sample07" $ do
            let bns = array (Red,Black) [(Red,1),(Green,10),(Blue,3),(Yellow,4),(Black,16)]
            processBlockTarget graph_nodes graph_edges bns [(Blue,2)] Yellow 5 (StartPoint 5) (EndPoint 11) `shouldBe` []
            
        it "sample08" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            [calcOptimizedRootTarget graph_nodes graph_edges bns [array (Red,Black) [(Red,1),(Green,10),(Blue,2),(Yellow,5),(Black,16)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 10) (EndPoint 11)] `shouldBe` []

    describe "toInitCode" $ do
        it "sample01" $ do
            toInitCode (array (Red,Black) [(Red,15),(Blue,14),(Yellow,12),(Black,13)])`shouldBe` (13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1)
        
        it "sample02" $ do
            let code = (13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1)
            fromInitCode 1 code `shouldBe` (array (Red,Black) [(Red,15),(Green,1),(Blue,14),(Yellow,12),(Black,13)])

        it "sample03" $ do
            let arr = array (Red,Black) [(Red,15),(Green,1),(Blue,14),(Yellow,12),(Black,13)]
            fromInitCode 1 (toInitCode arr) `shouldBe` arr

        it "sample04" $ do
            let code = (13-1)*11*11*11+(11-1)*11*11+(10-1)*11+(10-1)
            toInitCode (fromInitCode 1 code) `shouldBe` code
            