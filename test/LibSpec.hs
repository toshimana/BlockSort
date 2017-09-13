module LibSpec (spec) where

import Test.Hspec
import Data.Array
import Data.List
import Data.Maybe

import Lib
import BlockColor
import InitCode
import GraphConstants
import BonusPoint

spec :: Spec
spec = do
    describe "solve" $ do
        it "sample09" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            calcTargetRoot (StartPoint 17) (EndPoint 18) bns `shouldBe` [(BonusPoint 23,Cost 1201.118,[17,10,40,12,42,13,43,14,43,13,37,8,30,5,22,1,19,23,24,6,24,2,24,31,8,31,32,25,3,25,32,9,32,33,7,27,4,27,7,33,39,38,43,13,43,38,44,15,41,11,18],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)])]
            
        it "sample10" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            getAnswerList (StartPoint 17) (EndPoint 18) bns 20 `shouldBe` Just (BonusPoint 20,Cost 1025.438,[17,28,1,19,23,24,6,25,26,7,35,11,35,7,26,3,25,6,25,24,2,20,3,26,7,26,27,4,21,3,21,26,25,6,25,26,27,4,27,29,18],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)])
        
        it "sample11" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            let ls = fromJust (find (\xs -> fst (head xs) == 20) answerList)
            let (_,xs) = ls !! 17
            calcOptimizedRootTarget bns [xs] (StartPoint 17) (EndPoint 18) `shouldBe` Just ([17,28,1,19,23,24,6,25,26,7,35,11,35,7,26,3,25,6,25,24,2,20,3,26,7,26,27,4,21,3,21,26,25,6,25,26,27,4,27,29,18],Cost 1025.438,array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)])

        it "sample12" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,10)]
            [calcOptimizedRootTarget bns [array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 17) (EndPoint 11)] `shouldBe` [Nothing]

        it "sample13" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            processBlockTarget bns [(Red,1)] Blue 2 (StartPoint 10) (EndPoint 11) `shouldBe` [([10,28,1,28,10,28,34,30,23,2,19,1,19,22,28,10,34,30,23,2,23,24,6,25,26,7,35,11],Cost 605.315,array (Red,Black) [(Red,1),(Green,3),(Blue,2),(Yellow,4),(Black,5)])]
            
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
    
    describe "isDeadLock" $ do
        it "sample01" $ do
            let arr = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,5),(Black,4)]
            isDeadLock [] [(Blue,2),(Red,1)] arr 2 `shouldBe` True

        it "sample02" $ do
            let arr = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,5),(Black,4)]
            isDeadLock [2] [(Blue,2),(Red,1)] arr 1 `shouldBe` True            