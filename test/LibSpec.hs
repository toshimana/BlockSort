module LibSpec (spec) where

import Test.Hspec
import Data.Array
import Data.List as L
import Data.Map as M
import Data.Maybe

import Lib
import BlockColor
import InitCode
import GraphConstants
import BonusPoint

spec :: Spec
spec = do
    describe "createRotateBaseEdges" $ do
        it "sample01" $ do
            let (g,e,_,_,m,i) = createRotateBaseEdges graph_nodes graph_edges
            (g,e,m,i) `shouldBe` (graph_nodes,graph_edges,M.empty,length graph_nodes)

    describe "addMiniEdges" $ do
        it "sample01" $ do
            let (nodesWithMiniNodes,edgesHavingMiniNodes,nodeToOuterMiniNode,nodeToInnerMiniNode,miniNodeToParentNode,sizeOfNodes) = createRotateBaseEdges graph_nodes graph_edges
            addMiniEdges edgesHavingMiniNodes calcDepartCostFromAngle nodeToOuterMiniNode nodeToInnerMiniNode `shouldBe` []

    describe "addParentEdges" $ do
        it "sample01" $ do
            let (nodesWithMiniNodes,edgesHavingMiniNodes,nodeToOuterMiniNode,nodeToInnerMiniNode,miniNodeToParentNode,sizeOfNodes) = createRotateBaseEdges graph_nodes graph_edges
            let edgesWithMiniEdges = addMiniEdges edgesHavingMiniNodes calcDepartCostFromAngle nodeToOuterMiniNode nodeToInnerMiniNode
            addParentEdges edgesWithMiniEdges nodeToOuterMiniNode nodeToInnerMiniNode (StartPoint 17) (EndPoint 18) `shouldBe` []
    
    describe "refinePath" $ do
        it "sample01" $ do
            let (nodesWithMiniNodes,edgesHavingMiniNodes,nodeToOuterMiniNode,nodeToInnerMiniNode,miniNodeToParentNode,sizeOfNodes) = createRotateBaseEdges graph_nodes graph_edges
            let edgesWithMiniEdges = addMiniEdges edgesHavingMiniNodes calcDepartCostFromAngle nodeToOuterMiniNode nodeToInnerMiniNode
            let edgesWithParentEdges = addParentEdges edgesWithMiniEdges nodeToOuterMiniNode nodeToInnerMiniNode (StartPoint 17) (EndPoint 18)
            let g = createGraph nodesWithMiniNodes edgesWithParentEdges
            searchShortPath (StartPoint 17) (EndPoint 18) g `shouldBe` Nothing
            

    describe "goto" $ do
        it "sample01" $ do
            goto [] calcDepartCostFromAngle (StartPoint 17) (EndPoint 18) `shouldBe` Nothing

    describe "solve" $ do
        it "sample09" $ do
            let bns = array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            calcTargetRoot (StartPoint 17) (EndPoint 18) bns `shouldBe` [(BonusPoint 23,Cost 70.24862,[10,12,13,14,13,8,6,2,5,8,5,1,2,6,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 84.10497,[10,12,13,14,13,8,6,2,5,8,5,2,3,7,9,7,3,2,1,2,6,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 69.077065,[10,12,13,14,13,8,6,2,5,8,5,2,3,7,9,7,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 70.6848,[10,12,13,14,13,8,6,2,5,8,5,2,6,9,7,4,7,9,6,2,5,10,12,13,12,10,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 86.24862,[10,12,13,14,13,8,6,2,5,8,5,2,6,9,7,4,7,9,6,2,5,10,12,13,12,10,5,2,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 74.78453,[10,12,13,14,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 77.176796,[10,12,13,14,13,8,6,3,6,9,6,2,6,8,6,2,1,2,6,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 69.077065,[10,12,13,14,13,8,6,3,6,9,6,2,6,8,6,3,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 82.24862,[10,12,13,14,13,8,6,3,6,9,6,8,5,1,5,8,6,8,5,2,5,8,5,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 66.24862,[10,12,13,14,13,8,6,3,6,9,6,3,4,3,6,8,13,8,5,2,6,8,6,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 67.32044,[10,12,13,14,13,8,6,3,6,9,6,3,4,3,6,8,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 59.856354,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,2,5,8,5,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 74.24862,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,2,5,8,5,2,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 64.392265,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,9,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 70.78453,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,6,3,7,9,7,3,2,6,8,6,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 76.392265,[10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,6,3,7,9,7,3,6,8,5,1,5,8,6,8,5,2,5,8,5,2,3,7,11],array (Red,Black) [(Red,8),(Green,6),(Blue,9),(Yellow,13),(Black,14)]),(BonusPoint 23,Cost 72.78453,[10,5,8,5,2,6,9,14,9,6,2,1,2,6,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 79.71271,[10,5,8,5,2,6,9,14,9,6,3,7,9,7,3,2,1,2,6,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 64.6848,[10,5,8,5,2,6,9,14,9,6,3,7,9,7,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 61.756622,[10,5,8,5,2,6,9,14,9,7,4,7,9,6,2,5,10,12,13,12,10,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 77.32044,[10,5,8,5,2,6,9,14,9,7,4,7,9,6,2,5,10,12,13,12,10,5,2,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 94.54115,[10,5,8,5,1,10,12,13,14,9,6,9,14,13,12,10,5,2,5,10,12,13,14,13,12,10,5,2,3,7,9,7,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 94.00524,[10,5,8,5,1,10,12,13,14,9,6,9,7,3,7,9,7,3,2,5,10,12,13,14,13,12,10,5,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 70.97733,[10,5,8,5,1,10,12,13,14,9,6,9,7,3,7,9,7,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 68.14889,[10,5,8,5,1,10,12,13,14,9,6,9,7,4,7,9,14,13,14,9,7,3,7,9,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 90.00524,[10,5,8,5,10,12,13,14,9,7,3,6,9,6,2,5,10,12,13,14,13,12,10,1,2,6,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 90.00524,[10,5,8,5,10,12,13,14,9,7,3,6,9,6,2,5,10,12,13,14,13,12,10,5,2,3,4,3,2,5,10,12,13,12,10,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 123.76186,[10,5,8,5,10,12,13,14,9,7,3,6,9,6,3,7,11,15,14,13,12,10,1,10,12,13,14,15,11,7,3,6,3,2,5,10,12,13,14,13,12,10,5,2,3,4,3,2,5,10,12,13,12,10,5,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 104.198044,[10,5,8,5,10,12,13,14,9,7,3,6,9,6,3,7,11,15,14,13,12,10,1,10,12,13,14,15,11,7,3,6,3,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 93.46933,[10,5,8,5,10,12,13,14,9,7,3,6,9,6,3,4,11,15,14,13,14,15,11,7,3,2,3,7,11,15,14,15,11,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 68.78453,[10,5,8,5,10,12,13,14,9,7,4,7,9,14,13,14,9,6,2,6,9,14,9,6,2,1,2,6,2,3,7,9,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 76.24862,[10,5,8,5,10,12,13,14,9,7,4,7,9,14,13,14,9,6,2,6,9,14,9,6,3,7,9,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)]),(BonusPoint 23,Cost 87.176796,[10,5,8,5,10,12,13,14,9,7,4,7,9,14,13,14,9,7,3,7,9,7,3,2,3,7,11,15,14,15,11,7,3,2,1,2,6,2,3,7,11],array (Red,Black) [(Red,14),(Green,6),(Blue,9),(Yellow,13),(Black,8)])]
            
        it "sample11" $ do
            let ic = toInitCode $ array (Red,Black) [(Red,2),(Green,1),(Blue,3),(Yellow,4),(Black,10)]
            createRootFromCode 1 4000 ic `shouldBe` [10,12,13,14,13,8,6,9,7,4,7,9,6,8,13,8,5,2,5,8,5,1,2,6,2,3,7,9,7,11]

        it "sample12" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,10)]
            [calcOptimizedRootTarget bns [array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,10)],array (Red,Black) [(Red,7),(Green,11),(Blue,4),(Yellow,3),(Black,16)]] (StartPoint 17) (EndPoint 18)] `shouldBe` [Nothing]

        it "sample13" $ do
            let bns = array (Red,Black) [(Red,2),(Green,3),(Blue,1),(Yellow,4),(Black,5)]
            processBlockTarget bns [(Red,1)] Blue 2 (StartPoint 17) (EndPoint 18) `shouldBe` [([10,1,10,1,2,1,2,6,8,12,10,12,8,6,2,6,9,7,11],Cost 43.513245,array (Red,Black) [(Red,1),(Green,3),(Blue,2),(Yellow,4),(Black,5)])]

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