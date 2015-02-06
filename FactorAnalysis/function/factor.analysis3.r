### R:sample variance or similarity matrix
### m:# of main factors
factor.analysis3 <- function(S, m, d){

    p <- nrow(S)
    diag_S <- diag(S)
    sum_rank <- sum(diag_S)
    
    rowname <- paste("X", 1:p, sep="")
    colname <- paste("Factor", 1:m, sep=".")
    A <- matrix(0, p, m, dimnames=list(rowname, colname))
    
    kmax <- 20
    k <- -1
    repeat{
      
      d1 <- d
      d2 <- 1/sqrt(d)
      eig <- eigen(S*(d2 %o% d2))
      
      for(j in 1:m){
        A[,j] <- sqrt(eig$values[j]-1) * eig$vectors[,j]
      }
      A <- diag(sqrt(d)) %*% A
      d <- diag(S - A %*% t(A))
      
      if((sqrt(sum(d-d1)^2)) < 1e-4 | k==kmax){
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
    
    method <- c("Maximum Likelihood Method")
    list(method=method,
         loadings=A,
         var=cbind(common=diag_S-d, specific=d),
         B=B,
         iterative=k)
}

