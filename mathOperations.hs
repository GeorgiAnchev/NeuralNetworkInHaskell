module MathOperations where

import Data.List;
import Debug.Trace;

debug = flip trace

dot :: [Double] -> [Double] -> Double
dot vec1 vec2 = 
    sum (zipWith (*) vec1 vec2)

mvMult :: [[Double]] -> [Double] -> [Double]
mvMult mat vec = 
    map (dot vec) mat

mmMult :: [[Double]] -> [[Double]] -> [[Double]]
mmMult mat1 mat2 = 
    map (mvMult (transpose mat2)) mat1

mmSum :: [[Double]] -> [[Double]] -> [[Double]]
mmSum mat1 mat2 = zipWith vecSum mat1 mat2

multVecByScalar :: [Double] -> Double -> [Double]
multVecByScalar vec scalar = map (*scalar) vec

multMatByScalar :: [[Double]] -> Double -> [[Double]]
multMatByScalar mat scalar = map (`multVecByScalar` scalar) mat

divMatByVec :: [[Double]] -> [Double] -> [[Double]]
divMatByVec mat vec = zipWith divideVecByScalar mat vec

divideVecByScalar :: [Double] -> Double -> [Double]
divideVecByScalar vec value = map (/value) vec

divMatByScalar :: [[Double]] -> Double -> [[Double]]
divMatByScalar mat value = map (`divideVecByScalar` value) mat

subtractMatElem ::[[Double]] -> [[Double]] -> [[Double]]
subtractMatElem mat1 mat2 = zipWith subtractVecElem mat1 mat2

subtractVecElem :: [Double] -> [Double] -> [Double]
subtractVecElem vec1 vec2 = vec1 `vecSum` (vec2 `multVecByScalar` (-1))

broadcastSum ::  [[Double]] -> [Double] -> [[Double]]
broadcastSum mat vec = map (`vecSum` vec) mat 

vecSum :: [Double] -> [Double] -> [Double]
vecSum vec1 vec2 = zipWith (+) vec1 vec2

reLUmat :: [[Double]] -> [[Double]]
reLUmat mat = map (\row -> reLUvec row) mat

reLUmatWithAnotherMatAsCondition :: [[Double]] -> [[Double]] -> [[Double]]
reLUmatWithAnotherMatAsCondition mat1 mat2 = zipWith reLUvecWithAnotherVecAsCondition mat1 mat2

reLUvecWithAnotherVecAsCondition :: [Double] -> [Double] -> [Double]
reLUvecWithAnotherVecAsCondition vec1 vec2 =  zipWith (\s1 s2 -> if s2 > 0 then s1 else 0) vec1 vec2 

reLUvec :: [Double] -> [Double]
reLUvec vec = map ((max) 0) vec

expMat :: [[Double]] -> [[Double]]
expMat mat = map expVec mat

expVec :: [Double ] -> [Double]
expVec v = map exp v

getSpecificIndices :: [[Double]] -> [Int] -> [Double]
getSpecificIndices mat indices = zipWith (\row i -> row !! i) mat indices

divideElem ::  [Double] -> [Double] -> [Double]
divideElem vec1 vec2 = zipWith (/) vec1 vec2

sqrMatElem :: [[Double]] -> [[Double]]
sqrMatElem mat1 = map (\row -> map (**2) row) mat1

sumMat ::  [[Double]] -> Double
sumMat mat = sum $ map sum mat

sumByCol :: [[Double ]] -> [Double]
sumByCol mat = map sum $ transpose mat

setAtIndices :: [[Double]] -> [Int] -> Double -> [[Double]]
setAtIndices mat vec value = zipWith (\row i -> setAtIndex row i value) mat vec 

setAtIndex :: [Double] -> Int -> Double -> [Double]
setAtIndex vec index value = l ++ [value] ++ r 
    where (l, _ : r) = splitAt index vec 

maxIndex xs = head $ filter ((== maximum xs) . (xs !!)) [0..]

getAccuracy :: [Int] -> [Int] -> Double
getAccuracy predictions y = sum matches / (fromIntegral (length matches) :: Double)
    where matches = zipWith (\pred real -> if pred == real then 1 else 0) predictions y


