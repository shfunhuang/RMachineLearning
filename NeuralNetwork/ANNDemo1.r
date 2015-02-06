rm(list=ls())
setwd("E:/Dropbox/Statistics/Function")


#install.packages("neuralnet")
#install.packages("psych")
library(neuralnet)
library(psych)

source("normalize.r")
#source("beta.int.r")
concrete <- read.csv("E:/Dropbox/Statistics/Neural Network/concrete.csv")
#write.csv(t(concrete), "concrete1.csv")

str(concrete)
dim(concrete)
class(concrete)

concrete_norm <- as.data.frame(lapply(concrete, normalize))
#write.csv(t(concrete_norm, "concrete_norm.csv")
summary(concrete_norm$strength)
summary(concrete$strength)

concrete_norm <- concrete_norm[sample(1:nrow(concrete_norm), nrow(concrete_norm)),]
c.index <- sample(0:1, nrow(concrete_norm), prob=c(0.3, 0.7), rep=T)
concrete_train <- concrete_norm[c.index==1, ]
concrete_test <- concrete_norm[c.index==0, ]
dim(concrete_train) #70%
dim(concrete_test) #30%

### Single Hidden Node
concrete_model <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, hidden=c(3,2,1), data=concrete_train)
names(concrete_model)
plot(concrete_model)
#plot(neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, data=concrete_train))
#plot.nn(concrete_model)

### Evaluating Model Performance
model_result <- compute(concrete_model, concrete_test[,1:8])
#neurons, which stores the neurons for each layer in the network
#net.result, which stores the predicted values
names(model_result) 
predicted_strength <- model_result$net.result
#cbind(predicted_strength, concrete_test$strength)
cor(predicted_strength, concrete_test$strength) #0.7198275054

##############################################################################################

### Train By Linear Regression
attach(concrete)
#hist(strength) #like a normal distribution
#pairs(concrete_train)
cor(concrete)
pairs.panels(concrete)
concrete_lm <- lm(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age)
concrete_glm <- glm(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, family=gaussian)
#summary(concrete_lm) #Adjusted R-squared: 0.6125073
#summary(concrete_glm)
plot(residuals(concrete_lm))

#add square term
concrete_lm_new <- update(concrete_lm, .~.+I(slag^2)+I(ash^2)+I(superplastic^2)+I(age^2))
#summary(concrete_lm_new) #Adjusted R-squared: 0.764629
#beta.int(concrete_lm_new)

#minus x term(un-significance)
concrete_lm_new2 <- update(concrete_lm_new, .~.-coarseagg-fineagg)
summary(concrete_lm_new2) #Adjusted R-squared: 0.7649353


### Step Regression 
concrete_lm_step <- step(concrete_lm) 
#summary(concrete_lm_step) #Adjusted R-squared: 0.6125073 
concrete_lm_step2 <- step(concrete_lm_new)
#summary(concrete_lm_step2) #Adjusted R-squared: 0.7649353
concrete_lm_step3 <- step(concrete_lm_new2)
summary(concrete_lm_step3) #Adjusted R-squared: 0.7649353

##############################################################################################

traininginput <-  as.data.frame(runif(50, min=0, max=100))
tainingoutput <- sqrt(traininginput)
trainingdata <- cbind(traininginput,tainingoutput)
colnames(trainingdata) <- c("Input","Output")
net.sqrt <- neuralnet(Output~Input,trainingdata, hidden=10, threshold=0.01)
print(net.sqrt)
plot(net.sqrt)

testdata <- as.data.frame((1:10)^2)
net.results <- compute(net.sqrt, testdata)
ls(net.results)
print(net.results$net.result)
cleanoutput <- cbind(testdata,sqrt(testdata),as.data.frame(net.results$net.result))
colnames(cleanoutput) <- c("Input","Expected Output","Neural Net Output")
print(cleanoutput)

##############################################################################################
data(infert)
nn <- neuralnet(case~age+parity+induced+spontaneous, data=infert, 
                hidden=2, 
                err.fct="ce", 
                linear.output=F, 
                likelihood=T,
                rep=1)
names(nn)
nn$result.matrix
#nn$net.result

out <- cbind(nn$covariate, nn$net.result[[1]], nn$net.result[[2]])
dimnames(out) <- list(NULL, c("age", "parity", "induced", "spontaneous", "nn-output1", "nn-output2"))
head(out)
plot(nn)

### Traditional Backpropagation
library(nnet)
nn.bp <- neuralnet(case~age+parity+induced+spontaneous, data=infert,
                   hidden=2,
                   err.fct="ce",
                   linear.output=FALSE,
                   algorithm="backprop",
                   learningrate=0.01)

nn.nnet <- nnet(case~age+parity+induced+spontaneous, data=infert,
                size=2,
                entropy=T,
                abstol=0.01)


### plot generalized weights
head(nn$generalized.weights[[1]])
par(mfrow=c(2,2))
gwplot(nn, selected.covariate="age",min=-2.5,max=5)
gwplot(nn, selected.covariate="parity",min=-2.5,max=5)
gwplot(nn, selected.covariate="induced",min=-2.5,max=5)
gwplot(nn, selected.covariate="spontaneous",min=-2.5,max=5)



