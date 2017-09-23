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
    describe "goto" $ do
        it "sample01" $ do
            goto [] calcDepartCostFromAngle (StartPoint 17) (EndPoint 18) `shouldBe` Just ([17,125,124,103,106,111,114,115,118,119,122,109,108,127,130,18],Cost 10.130755)

    describe "solve" $ do
        it "sample09" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            calcTargetRoot (StartPoint 17) (EndPoint 18) bns `shouldBe` [(BonusPoint 23,Cost 99.82644,[10,12,8,12,10,5,2,6,9,14,9,6,2,1,2,6,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 104.730354,[10,12,8,12,10,5,2,6,9,14,9,7,3,6,9,6,2,1,2,6,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 93.75675,[10,12,8,12,10,5,2,6,9,14,9,7,3,6,9,6,3,4,7,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 87.73523,[10,12,8,12,10,5,2,6,9,14,9,7,4,7,9,6,2,5,10,12,13,12,10,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 107.18794,[10,12,8,12,10,5,2,6,9,14,9,7,4,7,9,6,2,5,10,12,13,12,10,5,2,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 125.91014,[10,12,8,12,10,1,10,12,13,14,9,6,9,14,13,12,10,5,2,5,10,12,13,14,13,12,10,5,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 126.31406,[10,12,8,12,10,1,10,12,13,14,9,6,9,7,3,7,9,7,3,2,5,10,12,13,14,13,12,10,5,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 102.84631,[10,12,8,12,10,1,10,12,13,14,9,6,9,7,3,7,9,7,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 101.17964,[10,12,8,12,10,1,10,12,13,14,9,6,9,7,4,11,15,14,13,14,9,7,3,7,9,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 107.74836,[10,12,8,12,13,14,9,7,3,6,9,6,2,5,10,12,13,14,13,12,10,1,2,6,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 107.74836,[10,12,8,12,13,14,9,7,3,6,9,6,2,5,10,12,13,14,13,12,10,5,2,3,4,7,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 148.24223,[10,12,8,12,13,14,9,7,3,6,9,6,3,7,11,15,14,13,12,10,1,10,12,13,14,15,11,7,3,6,3,2,5,10,12,13,14,13,12,10,5,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 127.19805,[10,12,8,12,13,14,9,7,3,6,9,6,3,7,11,15,14,13,12,10,1,10,12,13,14,15,11,7,3,6,3,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 115.13601,[10,12,8,12,13,14,9,7,3,6,9,6,3,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 91.02875,[10,12,8,12,13,14,9,7,4,11,15,14,13,14,9,6,2,6,9,14,9,6,2,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 95.52875,[10,12,8,12,13,14,9,7,4,11,15,14,13,14,9,6,2,6,9,14,9,7,3,6,9,6,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 105.17778,[10,12,8,12,13,14,9,7,4,11,15,14,13,14,9,7,3,6,9,6,2,3,7,11,15,14,15,11,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 96.08658,[10,12,13,14,13,8,5,2,6,8,6,2,1,2,6,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 104.43956,[10,12,13,14,13,8,5,2,6,8,6,3,7,9,7,3,2,1,2,6,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 84.58293,[10,12,13,14,13,8,5,2,6,8,6,3,7,9,7,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 88.647,[10,12,13,14,13,8,5,2,6,8,6,9,7,4,7,9,6,2,5,10,12,13,12,10,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 108.09972,[10,12,13,14,13,8,5,2,6,8,6,9,7,4,7,9,6,2,5,10,12,13,12,10,5,2,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 100.54729,[10,12,13,14,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 99.08658,[10,12,13,14,13,8,6,3,7,9,7,3,2,5,8,5,1,2,6,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 96.74835,[10,12,13,14,13,8,6,3,7,9,7,3,2,5,8,5,2,3,4,7,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 115.42116,[10,12,13,14,13,8,6,3,7,9,7,3,6,8,5,1,5,8,6,8,5,2,5,8,5,2,3,4,7,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 85.4931,[10,12,13,14,13,8,6,3,7,9,7,4,3,6,8,13,8,5,2,6,8,6,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 89.95381,[10,12,13,14,13,8,6,3,7,9,7,4,3,6,8,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 88.21071,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,2,6,8,6,2,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 96.159775,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,2,6,8,6,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 92.67142,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 90.80679,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,6,3,7,9,7,3,2,5,8,5,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 107.14137,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,6,3,7,9,7,3,6,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)])]
            
        it "sample11" $ do
            let ic = toInitCode $ array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            createRootFromCode 1 4000 ic `shouldBe` [10,12,13,14,13,8,5,2,6,8,6,3,7,9,7,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11]

        it "sample12" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,10)]
            [calcOptimizedRootTarget bns [array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 17) (EndPoint 18)] `shouldBe` [Nothing]

        it "sample13" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            let t = [array (Red,Black) [(Red,1),(Green,3),(Blue,2),(Yellow,4),(Black,5)]] :: [BlockPosition]
            processBlockTarget bns [Red] t Blue 2 (StartPoint 17) (EndPoint 18) `shouldBe` [([17,125,124,29,28,27,30,29,28,19,22,21,20,19,22,39,42,71,74,87,90,105,104,103,106,89,88,73,72,41,40,39,42,75,78,99,102,109,108,127,130,18],Cost 64.91813,array (Red,Black) [(Red,1),(Green,3),(Blue,2),(Yellow,4),(Black,5)])]

        it "sample14" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            let t = [array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,7)]] :: [BlockPosition]
            processBlockTarget bns [] t Black 7 (StartPoint 17) (EndPoint 18) `shouldBe` [([17,125,124,69,68,63,66,73,72,75,78,81,80,79,82,99,102,109,108,127,130,18],Cost 21.166666,array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,7)])]

        it "sample15" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            let t = [array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]] :: [BlockPosition]
            solveTarget bns [] t (StartPoint 17) (EndPoint 18) `shouldBe` [([17,125,124,103,106,111,114,115,118,119,122,109,108,127,130,18],Cost 10.130755,array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)])]

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
            isDeadLock [] arr [(Blue,2),(Red,1)] 2 `shouldBe` True

        it "sample02" $ do
            let arr = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,5),(Black,4)]
            isDeadLock [2] arr [(Blue,2),(Red,1)] 1 `shouldBe` True            