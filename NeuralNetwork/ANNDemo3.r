rm(list=ls())
setwd("E:/Dropbox/Statistics/")
library(neuralnet)
library(psych)
library(MASS)
library(RSNNS)
library(caret)
library(ggplot2)

source("normalize.r")

data(diamonds)
str(diamonds)
diamonds_norm <- diamonds

#we take the factor variables and turn them into numeric labels
diamonds_norm[,2] <- toNumericClassLabels(diamonds[,2]) #cut
diamonds_norm[,3] <- toNumericClassLabels(diamonds[,3]) #color
diamonds_norm[,4] <- toNumericClassLabels(diamonds[,4]) #clarity
#Because neural networks operate in terms of 0 to 1, or -1 to 1, 
#we must first normalize the price variable to 0 to 1, 
#making the lowest value 0 and the highest value 1.
diamonds_norm[,1] <- normalize(diamonds_norm[,1])
diamonds_norm[,2] <- normalize(diamonds_norm[,2])
diamonds_norm[,3] <- normalize(diamonds_norm[,3])
diamonds_norm[,4] <- normalize(diamonds_norm[,4])
diamonds_norm[,7] <- normalize(diamonds[,7])
#diamonds_norm[,7] <- normalizeData(diamonds[,7], type="0_1") #price
#diamonds_norm <- as.data.frame(lapply(diamonds, normalize))

dim(diamonds_norm) #53940 10
summary(diamonds_norm[,7])
summary(diamonds[,7])

diamonds_norm <- diamonds_norm[sample(1:nrow(diamonds_norm), nrow(diamonds_norm)),]
d.index <- sample(0:1, nrow(diamonds_norm), prob=c(0.3, 0.7), rep=T)
d.train <- diamonds_norm[d.index==1, c(-5,-6)]
d.test <- diamonds_norm[d.index==0, c(-5,-6)]
dim(d.train) #70%
dim(d.test) #30%

### Single Hidden Node
start.time <- Sys.time()
diamonds_model <- neuralnet(price~carat+cut+color+clarity, hidden=c(3,2), data=d.train)
names(diamonds_model)
end.time <- Sys.time()
end.time - start.time
plot(diamonds_model)

### Evaluating Model Performance
model_result <- compute(diamonds_model, d.test[,c(1,2,3,4)])
names(model_result) 
#neurons, which stores the neurons for each layer in the network
#net.result, which stores the predicted values

predicted_price <- model_result$net.result
#cbind(predicted_strength, d.test$price)
cor(predicted_price, d.test$price) #0.7198275054


