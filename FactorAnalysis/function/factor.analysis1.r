### S:sample variance or similarity matrix
### m:# of main factors
factor.analysis1 <- function(S, m){
  
  p <- nrow(S)
  diag_S <- diag(S)
  sum_rank <- sum(diag_S)
  
  rowname <- paste("X", 1:p, sep="")
  colname <- paste("Factor", 1:m, sep="")
  A <- matrix(0, nrow=p, ncol=m, dimnames=list(rowname, colname)) 
  
  eig <- eigen(S)
  
  for(j in 1:m){
    A[,j] <- sqrt(eig$values[j])*eig$vectors[,j]
  }
  
  h <- diag(A %*% t(A))
  
  rowname <- c("SS loadings", "Proportion Var", "Cumulative Var")
  B <- matrix(0, nrow=3, ncol=m, dimnames=list(rowname, colname))
  
  for(j in 1:m){
    B[1,j] <- sum(A[,j]^2)
    B[2,j] <- B[1,j]/sum_rank
    B[3,j] <- sum(B[1,1:j]/sum_rank)
  }
  
  method <- c("Principal Component Method")
  list(method=method, 
       loadings=A, 
       var=cbind(common=h, specific=diag_S-h), 
       B=B)
  
}

