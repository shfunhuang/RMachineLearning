rm(list=ls())
### Reference Neural Networks in R Using the Stuttgart Neural Network Simulator RSNNS.pdf ###
### Multi-Layer Perceptron for Classifcation

library(neuralnet)
library(psych)
library(MASS)
library(RSNNS)
library(caret)
library(ggplot2)
library(e1071)

data(iris) # data loaded
iris <- iris[sample(1:nrow(iris), length(1:nrow(iris))), 1:ncol(iris)] #data shuffled
irisValues <- iris[, 1:4]
irisTargets <- iris[, 5]

### Methods of pre- and post-processing

#generates a binary matrix from an integer-valued input vector representing class labels.
irisDecTargets <- decodeClassLabels(irisTargets)

#split into training and test set, $inputsTrain, $targetsTrain, $inputsTest, $targetsTest
iris <- splitForTrainingAndTest(irisValues, irisDecTargets, ratio = 0.15) #0.15 for test data

#normalization to zero mean and variance one
iris <- normTrainingAndTestSet(iris)

#cbind(iris$inputsTrain,iris$targetsTrain) #85%
#cbind(iris$inputsTest,iris$targetsTest)   #15%

#The training data of this structure can then be used for training the multi-layer perceptron

model <- mlp(iris$inputsTrain,                 #a matrix with training inputs for the network
             iris$targetsTrain,                #the corresponding targets values
             size = 5,                         #number of units in the hidden layer(s)
             maxit = 60,                       #maximum of iterations to learn
             learnFuncParams = 0.1,            #the parameters for the initialization function
             inputsTest = iris$inputsTest,     #a matrix with inputs to test the network
             targetsTest = iris$targetsTest    #the corresponding targets for the test input
             )

predictions <- predict(model, iris$inputsTest)

par(mfrow=c(2,2))
#iterative and regression error plots can be used for analysis, 
#but the regression error plot is less informative than for a regression problem
#The iterative error plot of both training (black) and test (red) error.
plotIterativeError(model)
plotRegressionError(predictions[, 2], iris$targetsTest[, 2], pch = 3)

#receiver operating characteristics (ROC)
#ROC plots are usually used for the analysis of classifcation problems with two classes.
plotROC(fitted.values(model)[, 2], iris$targetsTrain[, 2])
plotROC(predictions[, 2], iris$targetsTest[, 2])

#confusionMatrix(targets, predictions)
confusionMatrix(encodeClassLabels(iris$targetsTrain), 
                encodeClassLabels(fitted.values(model)))
#cbind(encodeClassLabels(iris$targetsTrain),encodeClassLabels(fitted.values(model)))

confusionMatrix(encodeClassLabels(iris$targetsTest), 
                encodeClassLabels(predictions))

confusionMatrix(encodeClassLabels(iris$targetsTrain), 
                encodeClassLabels(fitted.values(model), method="402040", l=0.4, h=0.6))

weightMatrix(model)

