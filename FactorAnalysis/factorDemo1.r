rm(list=ls())
setwd("E:/Dropbox/Statistics/Function")
source("./function/factor.analysis1.r")
source("./function/factor.analysis2.r")
source("./function/factor.analysis3.r")
source("./function/factor.analysis.r")
#x:similarity matrix
x <- c(1.000,
       0.923, 1.000,
       0.841, 0.851, 1.000,
       0.756, 0.807, 0.870, 1.000,
       0.700, 0.775, 0.835, 0.918, 1.000,
       0.619, 0.695, 0.779, 0.846, 0.928, 1.000,
       0.633, 0.697, 0.787, 0.869, 0.935, 0.975, 1.000,
       0.520, 0.596, 0.705, 0.806, 0.866, 0.932, 0.943, 1.000)
names <- c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8")
R <- matrix(0, 8, 8, dimnames=list(names, names))

for(i in 1:8){
  for(j in 1:8){
    R[i,j] <- x[(i-1)*i/2+j]
    R[j,i] <- R[i,j]
  }
}

fa1 <- factor.analysis1(R, m=2)
#SSE
E1 <- R-fa1$loadings %*% t(fa1$loadings) - diag(fa1$var[,2])
sum(E1^2) #0.01349859

### factor.analysis2
#d:estimate of initial specific standard variance
d <- c(0.123, 0.112, 0.155, 0.116, 0.073, 0.045, 0.033, 0.095)
fa2 <- factor.analysis2(R, m=2, d)
#SSE
E2 <- R-fa2$loadings %*% t(fa2$loadings) - diag(fa2$var[,2])
sum(E2^2) #0.006551494

### factor.analysis3
fa3 <- factor.analysis3(R, 2, d)
#SSE
E3 <- R-fa3$loadings %*% t(fa3$loadings) - diag(fa3$var[,2])
sum(E3^2) #0.007974058

### switch method of factor.analysis1~3
fa4 <- factor.analysis(R,2,d)

### Orthogonal rotation
fa5 <- factor.analysis(R, m=2, method="princomp")
vm1 <- varimax(fa5$loadings, normalize=F)

fa6 <- factor.analysis(R, m=2, method="factor")
vm2 <- varimax(fa6$loadings, normalize=F)

fa7 <- factor.analysis(R, m=2, method="likelihood")
vm3 <- varimax(fa7$loadings, normalize=F)

### factor analysis computer function
fa8 <- factanal(factors=2, covmat=R)



