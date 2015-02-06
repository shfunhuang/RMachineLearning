rm(list=ls())

#source("D:/Dropbox/Statistics/Machine_Learning_with_R/ANN/ANNDemo.r")
#source("E:/Dropbox/Statistics/Machine_Learning_with_R/ANN/ANNDemo.r")
setwd("E:/Dropbox/Statistics/Machine_Learning_with_R/ANN")

#install.packages("neuralnet")
#install.packages("psych")
library(neuralnet)
library(psych)

source("normalize.r")

redwine <- read.csv("winequality-red.csv", sep=";")
dim(redwine)
#str(redwine)
#class(redwine)
#head(redwine)
#summary(redwine)


redwine.pr <- princomp(redwine[,1:11], cor=T)
#redwine.pr <- princomp(~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, cor=T, data=redwine)
summary(redwine.pr, loadings=T)

pre <- predict(redwine.pr)

screeplot(redwine.pr, type="l")
plot(redwine.pr)

biplot(redwine.pr)
barplot(sort(-redwine.pr$scores[,1]), TRUE) # barplot of Comp.1


### PCA??‰ç”¨
### PCA??†é??
load <- loadings(redwine.pr)
# 11??…æ?‡æ??
plot(load[,1:2])
text(load[,1], load[,2], adj=c(-0.4,0.3))

### PCA Regression
lm.sol <- lm(quality~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=redwine)
summary(lm.sol) #0.3561

redwine.pr <- princomp(~fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, cor=T, data=redwine)
summary(redwine.pr, loadings=T)

redwine$z1 <- pre[,1];redwine$z2 <- pre[,2];redwine$z3 <- pre[,3]
redwine$z4 <- pre[,4];redwine$z5 <- pre[,5];redwine$z6 <- pre[,6]
redwine$z7 <- pre[,7];redwine$z8 <- pre[,8];redwine$z9 <- pre[,9]
redwine$z10 <- pre[,10];redwine$z11 <- pre[,11]

lm.sol <- lm(quality~z1+z2+z3+z4+z5+z6+z7+z8+z9+z10+z11, data=redwine)
summary(lm.sol) #0.3561

table(redwine$quality)
pairs.panels(redwine[,1:11], bg=c("red","orange","yellow","green","blue","purple")[redwine$quality])
