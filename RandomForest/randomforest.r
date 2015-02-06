#rm(list=ls())
set.seed(Sys.Date())

library(randomForest)
library(ROCR)

source("./function/shuffler.r")

data(iris)
ind <- shuffler(iris,0.8,0.2)

iris.rf <- randomForest(Species~., ind$train, ntree= 50 ,nPerm= 10 ,mtry= 3 ,proximity=TRUE,importance=TRUE)
list(forest=iris.rf)

print(iris.rf)
importance(iris.rf)

iris.pred <- predict( iris.rf, ind$test )
table(observed <- ind$test[,"Species"], predicted=iris.pred )

# Reference: http://scg.sdsu.edu/rf_r/
# Reference: http://www.zilhua.com/629.html

