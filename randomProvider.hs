module RandomProvider where

import System.Random;

g = mkStdGen 0;
a = randoms g :: [Double];

getRandomList :: Int -> [Double]
getRandomList size = map (\x -> x*2 - 1) $ take size a; 

getRandomMatrix :: Int -> Int -> [[Double]]
getRandomMatrix rows elems = splitEvery elems $ getRandomList $ rows * elems;

splitEvery :: Int -> [a] -> [[a]]
splitEvery _ [] = []
splitEvery n list = first : splitEvery n rest
  where
    (first,rest) = splitAt n list

getListOf :: Int -> Double -> [Double]
getListOf size v = replicate size v

getMatrixOf :: Int -> Int -> Double -> [[Double]]
getMatrixOf rows cols value = replicate rows $ getListOf cols value