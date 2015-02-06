#rm(list=ls())
setwd("E:/Dropbox/Statistics/Function")
set.seed(Sys.time())

#install.packages("neuralnet")
#install.packages("psych")
library(neuralnet)
library(psych) #for logistic

source("normalize.r")
source("shuffler.r")
#source("beta.int.r")
concrete <- read.csv("E:/Dropbox/Statistics/Neural Network/concrete.csv")

str(concrete)
dim(concrete)
class(concrete)

concrete_norm <- as.data.frame(lapply(concrete, normalize))
summary(concrete_norm$strength)
summary(concrete$strength)

concrete_norm <- shuffler(concrete_norm, 0.7, 0.3)
concrete_train <- concrete_norm$train
concrete_test <- concrete_norm$test
dim(concrete_train) #70%
dim(concrete_test) #30%

### Single Hidden Node
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, hidden=c(3,1), data=concrete_train)
names(concrete_model)
#plot(concrete_model)
#plot(neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data=concrete_train))
#plot.nn(concrete_model)

### Evaluating Model Performance
model_result <- compute(concrete_model, concrete_test[,1:8])
#neurons, which stores the neurons for each layer in the network
#net.result, which stores the predicted values
names(model_result) 
predicted_strength <- model_result$net.result
#cbind(predicted_strength, concrete_test$strength)
cor(predicted_strength, concrete_test$strength)
cor(concrete_model$net.result[[1]], concrete_train$strength)
#######################################################################################################








