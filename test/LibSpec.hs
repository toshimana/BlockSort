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
        it "sample01" $ do
            let bns = array (Red,Red) [(Red,1)]
            solve graph_nodes graph_edges bns [] (StartPoint 10) (EndPoint 11) `shouldBe` []

        it "sample02" $ do
            let bns = array (Red,Red) [(Red,14)]
            solve graph_nodes graph_edges bns [] (StartPoint 14) (EndPoint 11) `shouldBe` []

        it "sample03" $ do
            let bns = array (Red,Red) [(Red,2)]
            solve graph_nodes graph_edges bns [(Red,2)] (StartPoint 10) (EndPoint 11) `shouldBe` []
