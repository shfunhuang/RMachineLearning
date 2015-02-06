rm(list=ls())
source("./function/distinguish.distance.r")
source("./function/distinguish.bayes.r")

x <- iris[,1:4]
g <- gl(3,50) #factor(rep(1:3,each=50))
d1 <- distinguish.distance(x,g)
b1 <- distinguish.bayes(x,g)
rbind(d1,b1)

