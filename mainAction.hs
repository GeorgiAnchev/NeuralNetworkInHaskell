import TwoLayerNet;
import RandomProvider;
import MathOperations;
import SampleData;


inputSize = 32 * 32 * 3;
hiddenSize = 250;
numClasses = 10;
numInputs = 100;

--std = 1e-1;

randomWeights = getRandomWeights inputSize hiddenSize numClasses;

--toyX = getRandomMatrix numInputs inputSize `multMatByScalar` (10::Double);

trainY :: [Int]
trainY = concat $ map (\i -> replicate 10 i ) [0..9] ;   --[0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,2,2,2,2,, ... ,8,8,9,9,9,9,9,9,9,9,9,]

testY :: [Int]
testY = [0,1,2,3,4,5,6,7,8,9];
--sampleReg :: Double
--sampleReg = 0.05

iterations :: Int
iterations = 100

decay :: Double
decay = 0.94

learningRate :: Double
learningRate = 1.5e-3

regularization :: Double
regularization = 0.27

trainTheNN :: IO NNParams
trainTheNN = do
    trainX <- trainData
    print "dataLoaded"
    let trainedWeights = train randomWeights trainX trainY learningRate decay regularization iterations 
    return trainedWeights

fakeX :: [[Double]]
fakeX = [[]]

-- we get loss of 2656.792170588558 for untrained weights
lossUntrained :: IO Double  
lossUntrained = do
    trainX <- trainData
    print "dataLoaded"
    let calculatedLoss = loss randomWeights trainX trainY regularization
    return calculatedLoss

--hiddenSize 50:
-- with 10 iterations, decay 0.95 we get loss of 2573.2012272172187
-- with 20 iterations, decay 0.95 we get loss of 2571.6506376918082
-- with 30 iterations, decay 0.95 we get loss of 2571.3904234861616 
-- with 20 iterations, decay 1    we get loss of 2571.228887131216
-- with 20 iterations, decay 1, learnRate 2e-1 loss : 2570.473378131214
-- with 30 iterations, decay 1, learnRate 2e-1 loss : 2570.3716655753615
-- with 30 iterations, decay 0.95, learnRate 2e-1 loss : 2570.572749028657
--hiddenSize 250:
-- untrained loss : 69573.37563088247
-- with 10 iterations, decay 0.94, learnRate 1.5e-3, reg 0.27 - loss: 68665.4854169582

lossTrained :: IO Double
lossTrained = do
    trainX <- trainData
    print "dataLoaded"
    trainedWeights <- trainTheNN
    let calculatedLoss = loss trainedWeights trainX trainY regularization
    return calculatedLoss


-- accuracy with 2 iterations  : 0.2
-- accuracy with 100 iterations  : pak 0.2 :D
accuracy = do
    testX <- testData
    print "dataLoaded"
    trainedNN <- trainTheNN
    let prediction = predict trainedNN testX 
    let toyAccuracy = getAccuracy prediction testY   
    return toyAccuracy