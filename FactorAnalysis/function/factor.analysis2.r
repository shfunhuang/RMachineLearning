### R:sample variance or similarity matrix
### m:# of main factors
factor.analysis2 <- function(R, m, d){

    p <- nrow(R)
    diag_R <- diag(R)
    sum_rank <- sum(diag_R)
    
    rowname <- paste("X", 1:p, sep="")
    colname <- paste("Factor", 1:m, sep=".")
    A <- matrix(0, p, m, dimnames=list(rowname, colname))
    
    kmax <- 20
    k <- -1
    h <- diag_R-d
    repeat{
      
      diag(R) <- h
      h1 <- h
      eig <- eigen(R)
      
      for(j in 1:m){
        A[,j] <- sqrt(eig$values[j])*eig$vectors[,j]
      }      
      h <- diag(A %*% t(A))
      
      if((sqrt(sum(h-h1)^2)) < 1e-4 | k==kmax){
        break
      }
      k <- k+1
      
    }
    
    rowname <- c("SS loadings", "Proportion Var", "Cumulative Var")
    B <- matrix(0, 3, m, dimnames=list(rowname, colname))
    
    for(j in 1:m){
      B[1,j] <- sum(A[,j]^2)
      B[2,j] <- B[1,j]/sum_rank
      B[3,j] <- sum(B[1,1:j])/sum_rank
    }
    method <- c("Principal Factor Method")
    list(method=method,
         loadings=A,
         var=cbind(common=h,specific=diag_R-h),
         B=B,
         iterative=k)
}

