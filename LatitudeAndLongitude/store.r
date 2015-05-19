### website check ###
### http://www.storyday.com/wp-content/uploads/2008/09/latlung_dis.html

### Great-circle distance calculations in R ###
### http://www.r-bloggers.com/great-circle-distance-calculations-in-r/

rm(list=ls())
setwd(getwd())
### Taiwan Mobile Store
### https://www.taiwanmobile.com/cs/public/storeAction.do?utm_source=4GLTE&utm_medium=localnav&utm_campaign=retailstores
store <- read.table("data/store.csv", sep=",", head=T, stringsAsFactors=F)
str(store)
store <- store[,2:6]

# Replace PSDL_CDE
store[,3] <- gsub("^[0-9]{3}", "", store[,3]) 
head(store)

# decimal degrees converted to radians
deg2rad <- function(deg) return(deg*pi/180)
# Spherical Law of Cosines (slc)
source("function/gcd.slc.r")
# split data
source("function/shuffler.r")

store_training <- shuffler(store, 0.5, 0.5)
store_train <- store_training$train
store_test <- store_training$test
#dim(store_train)[1]*dim(store_test)[1]

# the distance between stores by Latitude and Longitude
k <- 0
results <- matrix(0,dim(store_train)[1]*dim(store_test)[1],3)
for(i in seq(dim(store_train)[1])){
	for(j in seq(dim(store_test)[1])){
		k <- k + 1
		#print(paste(store_train[,1][i], store_test[,1][j], gcd.slc(store_train$Latitude[i], store_train$Longitude[i],store_test$Latitude[j], store_test$Longitude[j])))
		results[k,1] <- store_train[,1][i]
		results[k,2] <- store_test[,1][j]
		results[k,3] <- gcd.slc(store_train$Latitude[i], store_train$Longitude[i],store_test$Latitude[j], store_test$Longitude[j])
	}
}
write.table(results, "results.csv", sep=",", row.names=F, col.names=F)

result <- read.csv("results.csv", head=F, stringsAsFactors=F)
colnames(result) <- c("train", "test", "dist")

# set range of distance in <100, 100~200, >300 km
for(i in seq(dim(store_train)[1]*dim(store_test)[1])){
	if(result[,3][i] <= 100){
		result$one_hundred[i] <- 1
		result$two_hundred[i] <- 0
		result$three_hundred[i] <- 0
	}else if(result[,3][i] > 100 & result[,3][i] <= 200){
		result$one_hundred[i] <- 0
		result$two_hundred[i] <- 1
		result$three_hundred[i] <- 0
	}else{
		result$one_hundred[i] <- 0
		result$two_hundred[i] <- 0
		result$three_hundred[i] <- 1
	}
}

# Compute the number of stores in range of distance
jump <- seq(1, dim(store_train)[1]*dim(store_test)[1]+dim(store_test)[1], by=dim(store_test)[1])
result_count <- matrix(0, dim(result[jump-1,])[1], 3)
rownames(result_count) <- result[jump-1,]$train
colnames(result_count) <- c("#one_hundred", "#two_hundred", "#three_hundred")

for(i in seq(dim(store_train)[1])){
	result_count[i,] <- apply(result[jump[i]:(jump[i+1]-1),][,c(4,5,6)],2,sum)
}
result_count
