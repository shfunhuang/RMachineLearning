source("E:/Dropbox/Statistics/Neural Network/ANNDemo1_1.r")
# get weights from model
# get bias from model

head(concrete_train)[,1:8]
head(concrete_model$covariate)

w1 <- matrix(concrete_model$weights[[1]][[1]][,1][2:9]) #8 1
w2 <- matrix(concrete_model$weights[[1]][[1]][,2][2:9]) #8 1
w3 <- matrix(concrete_model$weights[[1]][[1]][,3][2:9]) #8 1

s1 <- concrete_model$weights[[1]][[1]][1,] #bias 1 
s2 <- concrete_model$weights[[1]][[2]][1,] #bias 2
s3 <- concrete_model$weights[[1]][[3]][1,] #bias 3

#sum(t(w1) %*% t(concrete_train[,1:8])) # sum of w1 and input var.
#sum(t(w2) %*% t(concrete_train[,1:8])) # sum of w2 and input var.
#sum(t(w3) %*% t(concrete_train[,1:8])) # sum of w3 and input var.

#dim(t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8])) 
#dim(t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8])+s1) 

hiddenLayer1.Neuron1 <- logistic(s1+t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[1,]
hiddenLayer1.Neuron1 <- data.frame(hiddenLayer1.Neuron1)

hiddenLayer1.Neuron2 <- logistic(s1+t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[2,]
hiddenLayer1.Neuron2 <- data.frame(hiddenLayer1.Neuron2)

hiddenLayer1.Neuron3 <- logistic(s1+t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[3,]
hiddenLayer1.Neuron3 <- data.frame(hiddenLayer1.Neuron3)
#head(cbind(hiddenLayer1.Neuron1, hiddenLayer1.Neuron2, hiddenLayer1.Neuron3))

### Hidden Layer 1
hiddenLayer1.Neurons <- t(logistic(s1+t(concrete_model$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[1:3,])
hiddenLayer1.Neurons <- data.frame(hiddenLayer1.Neurons)
#head(hiddenLayer1.Neurons)

### Hidden Layer 2
hiddenLayer2.Neurons <- t(logistic(s2+t(concrete_model$weights[[1]][[2]][2:4,]) %*% t(hiddenLayer1.Neurons)))
hidden.Layer2.Neurons <- data.frame(hiddenLayer2.Neurons)
#head(hiddenLayer2.Neurons)

### Hidden Layer 3 (Output Layer)
hiddenLayer3.Neurons <- t((s3+t(concrete_model$weights[[1]][[3]][2,]) %*% t(hiddenLayer2.Neurons)))
hidden.Layer3.Neurons <- data.frame(hiddenLayer3.Neurons)
#head(hiddenLayer3.Neurons)
#summary(hidden.Layer3.Neurons)

#head(cbind(concrete_train$strength, concrete_model$net.result[[1]], hiddenLayer3.Neurons))
cor(cbind(concrete_train$strength, concrete_model$net.result[[1]], hiddenLayer3.Neurons))
