rm(list=ls())
setwd("E:/Dropbox/Statistics/Function/")

#install.packages("neuralnet")
#install.packages("psych")
library(neuralnet)
library(psych)

source("normalize.r")
source("shuffler.r")

redwine <- read.csv("E:/Dropbox/Statistics/Neural Network/winequality-red.csv", sep=";")
#write.csv(redwine, "redwine.csv")
str(redwine)
dim(redwine)
class(redwine)
head(redwine)
summary(redwine)
dim(redwine)
redwine_norm <- as.data.frame(lapply(redwine, normalize))
#redwine_norm$quality <- scale(redwine$quality)
summary(redwine$quality)
summary(redwine_norm$quality)
barplot(table(redwine$quality),col=rainbow(6))

redwine_norm <- shuffler(redwine_norm, 0.7, 0.3) #1599 12

redwine_train <-  redwine_norm$train #1112 12
redwine_test <- redwine_norm$test #487 12

### Single Hidden Node
names(redwine)
redwine_model <- neuralnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, hidden=c(3,1) ,data=redwine_train)
names(redwine_model)
#plot(neuralnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine_train))
plot(redwine_model)

### Evaluating Model Performance
model_result <- compute(redwine_model, redwine_test[,1:11])

names(model_result) 
predicted_quality <- model_result$net.result
cor(predicted_quality, redwine_test[,12]) #0.5430133904

### Improving Model Performance
redwine_model2 <- neuralnet(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine_train, hidden=2)
plot(redwine_model2)
model_result2 <- compute(redwine_model2, redwine_test[,1:11])
predicted_quality2 <- model_result2$net.result
cor(predicted_quality2, redwine_test[,12]) #0.5706597446

### Fitting Generalized Linear Models
redwine_glm <- glm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine_train, family=poisson("log"))
summary(redwine_glm)





