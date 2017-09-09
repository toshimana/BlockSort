module InitCode (InitCode(..), toInitCode, fromInitCode)  where

import Data.Array
import Data.Maybe
import Data.Graph.Inductive.Graph

import BlockColor

type BlockPosition = Array BlockColor Node

newtype InitCode = InitCode Int deriving (Ix,Ord,Eq,Show)

redIndices :: [Maybe Int]
redIndices = [Nothing,Just 1,Just 2,Just 3,Just 4,Just 5,Nothing,Nothing,Just 6,Just 7,Just 8,Just 9,Just 10,Nothing,Just 11]

yellowIndices :: [Maybe Int]
yellowIndices = [Just 1,Just 2,Nothing,Just 3,Nothing,Just 4,Just 5,Just 6,Just 7,Just 8,Just 9,Just 10,Nothing,Just 11,Nothing]

blueIndices :: [Maybe Int]
blueIndices = [Just 1,Nothing,Just 2,Nothing,Just 3,Just 4,Just 5,Just 6,Nothing,Just 7,Just 8,Nothing,Just 9,Just 10,Just 11]

revIndices :: [Maybe Int] -> [Int]
revIndices indices = map snd $ filter (\(a,_) -> isJust a) $ zip indices [1..]

revRedIndices :: [Int]
revRedIndices = revIndices redIndices

revYellowIndices :: [Int]
revYellowIndices = revIndices yellowIndices

revBlueIndices :: [Int]
revBlueIndices = revIndices blueIndices

toInitCode :: BlockPosition -> InitCode
toInitCode bp = 
    let blackP = bp ! Black in
    let redP = fromJust $ redIndices !! ((bp ! Red)-1) in
    let yellowP = fromJust $ yellowIndices !! ((bp ! Yellow)-1) in
    let blueP = fromJust $ blueIndices !! ((bp ! Blue)-1) in
    InitCode $ (blackP-1)*11*11*11+(redP-1)*11*11+(yellowP-1)*11+(blueP-1)

fromInitCode :: Node -> InitCode -> BlockPosition
fromInitCode gp (InitCode code) = 
    let kp = div code (11*11*11) + 1 in
    let rp = revRedIndices !! (mod (div code (11*11)) 11) in
    let yp = revYellowIndices !! (mod (div code 11) 11) in
    let bp = revBlueIndices !! (mod code 11) in
    array (Red,Black) [(Red,rp),(Green,gp),(Blue,bp),(Yellow,yp),(Black,kp)]

