import TwoLayerNet;
import RandomProvider;
import MathOperations;
import ToyData;

inputSize = 4;
hiddenSize = 10;
numClasses = 3;
numInputs = 5;

std = 1e-1;

-- toyNet = getRandomWeights inputSize hiddenSize numClasses std;
toyNet = NNParams toyW1 toyB1 toyW2 toyB2
--toyX = getRandomMatrix numInputs inputSize `multMatByScalar` (10::Double);

toyY :: [Int]
toyY = [0, 1, 2, 2, 1];

toyScores = calcScores toyNet toyX

toyLoss = loss toyNet toyX toyY toyReg

toyGrads = grads toyNet toyX toyY toyReg

(w1grad, b1grad, w2grad, b2grad) = toyGrads

toyTrained = train toyNet toyX toyY toyX toyY (1e-1) 0.95 (5e-6) 100 --200

lossTrained = loss toyTrained toyX toyY toyReg

toyPrediction = predict toyTrained toyX 

toyAccuracy = getAccuracy toyPrediction toyY