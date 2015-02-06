#Split data with train and test dataset with p1 and p2
shuffler <- function(mydata, p1, p2){
  
  data.index <- sample(0:1, nrow(mydata), prob=c(p2, p1), rep=T)
  data_train <- mydata[data.index==1, ]
  data_test <- mydata[data.index==0, ]
  dim(data_train) #p1%
  dim(data_test) #p2%  
  list(train=data_train, test=data_test, p1=p1, p2=p2)
  
}

