module TwoLayerNet where

import RandomProvider;
import MathOperations;
import Data.List;

data NNParams = NNParams{
    w1 :: [[Double]],
    b1 :: [Double],
    w2 :: [[Double]],
    b2 :: [Double]
} deriving (Show)

getRandomWeights :: Int -> Int -> Int -> NNParams
getRandomWeights inputSize hiddenSize outputSize = NNParams w1 b1 w2 b2
    where 
        w1 = getRandomMatrix inputSize hiddenSize
        b1 = getListOf hiddenSize 0
        w2 = getRandomMatrix hiddenSize outputSize
        b2 = getListOf outputSize 0

calcScores :: NNParams -> [[Double]] -> [[Double]] 
calcScores (NNParams w1 b1 w2 b2) x = layer2 
    where 
        layer1 = (x `mmMult` w1) `broadcastSum` b1
        layer1activated = reLUmat layer1
        layer2 = (layer1activated `mmMult` w2) `broadcastSum` b2

loss :: NNParams -> [[Double]] -> [Int] -> Double -> Double 
loss (NNParams w1 b1 w2 b2) x y reg = rawLoss + regularizationLoss  
    where 
        regularizationLoss = reg * sumMat( sqrMatElem w1) + reg * sumMat(sqrMatElem w2)
        rawLoss = (negate $ sum $ map log yPredictions) / numTrain
        yPredictions = getSpecificIndices expScores y `divideElem` summedScores
        summedScores = map sum expScores
        expScores = expMat scores
        scores = calcScores (NNParams w1 b1 w2 b2) x
        numTrain = fromIntegral (length x) :: Double

grads (NNParams w1 b1 w2 b2) x y reg = (gradientW1, gradientB1, gradientW2, gradientB2) 
    where 
        summedScores = map sum expScores
        expScores = expMat scores 
        scores = calcScores (NNParams w1 b1 w2 b2) x

        gradientB1 = sumByCol dHiddenActivated
        gradientW1 = transpose x `mmMult` dHiddenActivated `mmSum` gradientW1Reg
        gradientW1Reg = w1 `multMatByScalar` (2 * reg) 
        dHiddenActivated = reLUmatWithAnotherMatAsCondition dHidden layer1
        dHidden = dscores `mmMult` transpose w2
        gradientB2 = sumByCol dscores
        gradientW2 = (transpose layer1activated `mmMult` dscores) `mmSum` gradientW2Reg
        gradientW2Reg = w2 `multMatByScalar` (2 * reg) --`debug` ("AAAAAAAAAAA" ++ show gradientW2Reg) 
        dscores = ((expScores `divMatByVec` summedScores) `subtractMatElem` classMask ) `divMatByScalar` numTrain
        classMask = setAtIndices classMaskZeroes y 1
        classMaskZeroes = getMatrixOf (length x) outputSize 0 --`debug` ("AAAAAAAAAAA" ++ show scores)
        layer1 = (x `mmMult` w1) `broadcastSum` b1
        layer1activated = reLUmat layer1
        outputSize = length b2
        numTrain = fromIntegral (length x) :: Double

train :: NNParams -> [[Double]] -> [Int] -> Double -> Double -> Double -> Int -> NNParams 
train (NNParams w1 b1 w2 b2) x y learningRate learningRateDecay reg numIters 
    | numIters == 0 = NNParams w1 b1 w2 b2
    | otherwise = train (NNParams newW1 newB1 newW2 newB2) x y (learningRate * learningRateDecay) learningRateDecay reg (numIters - 1 ) --batchSize
        where 
            newW1 = w1 `subtractMatElem` ( gradW1 `multMatByScalar` learningRate) 
            newB1 = b1 `subtractVecElem` ( gradB1 `multVecByScalar` learningRate) 
            newW2 = w2 `subtractMatElem` ( gradW2 `multMatByScalar` learningRate) 
            newB2 = b2 `subtractVecElem` ( gradB2 `multVecByScalar` learningRate) 
            (gradW1, gradB1, gradW2, gradB2) = grads (NNParams w1 b1 w2 b2) x y reg

predict (NNParams w1 b1 w2 b2) x = predictions 
    where 
        predictions = map maxIndex scores
        scores = calcScores (NNParams w1 b1 w2 b2) x