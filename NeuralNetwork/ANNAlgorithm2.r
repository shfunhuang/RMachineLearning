source("E:/Dropbox/Statistics/Neural Network/ANNDemo1_1.r")

### Start at Input Layer
### Hidden Layer 1
c.nn11 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, hidden=c(1), data=concrete_train)
s11 <- c.nn11$weights[[1]][[1]][1,] #bias 1
hiddenLayer1.Neurons <- logistic(s11+t(c.nn11$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[1,]
hiddenLayer1.Neurons <- data.frame(hiddenLayer1.Neurons)
#summary(hiddenLayer1.Neurons)
#plot(c.nn11)

c.nn12 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, hidden=c(1), data=concrete_train)
s12 <- c.nn12$weights[[1]][[1]][1,] #bias 2
hiddenLayer2.Neurons <- logistic(s12+t(c.nn12$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[1,]
hiddenLayer2.Neurons <- data.frame(hiddenLayer2.Neurons)
#plot(c.nn12)

c.nn13 <- neuralnet(strength~cement+slag+ash+water+superplastic+coarseagg+fineagg+age, hidden=c(1), data=concrete_train)
s13 <- c.nn13$weights[[1]][[1]][1,] #bias 1
hiddenLayer3.Neurons <- logistic(s13+t(c.nn13$weights[[1]][[1]][2:9,]) %*% t(concrete_train[,1:8]))[1,]
hiddenLayer3.Neurons <- data.frame(hiddenLayer3.Neurons)
#plot(c.nn13)

### Hidden Layers 2
hiddenLayer2 <- cbind(hiddenLayer1.Neurons,hiddenLayer2.Neurons,hiddenLayer3.Neurons, concrete_train$strength)
#summary(hiddenLayer2)
c.nn21 <- neuralnet(concrete_train$strength~hiddenLayer1.Neurons+hiddenLayer2.Neurons+hiddenLayer3.Neurons, hidden=c(1), data=hiddenLayer2)
s21 <- c.nn21$weights[[1]][[1]][1,]
hiddenLayer2.Neurons <- t(logistic(s21+t(c.nn21$weights[[1]][[1]][2:4,]) %*% t(hiddenLayer2[,1:3])))
hidden.Layer2.Neurons <- data.frame(hiddenLayer2.Neurons)
#summary(hidden.Layer2.Neurons)
#plot(c.nn21)

### Hidden Layer 3 (Output Layer)
s31 <- c.nn21$weights[[1]][[2]][1,]
hiddenLayer3.Neurons <- t((s31+t(c.nn21$weights[[1]][[2]][2,]) %*% t(hiddenLayer2.Neurons)))
hidden.Layer3.Neurons <- data.frame(hiddenLayer3.Neurons)
#summary(hidden.Layer3.Neurons)
#summary(c.nn21$net.result[[1]])
cor(cbind(concrete_train$strength, concrete_model$net.result[[1]], 
          c.nn21$net.result[[1]], hiddenLayer3.Neurons))



