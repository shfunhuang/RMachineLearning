# TrnX : train dataset
# TrnG : factor variable
# TstX : test data
# p : prior probability
distinguish.bayes <- function(TrnX, TrnG, p=rep(1,length(levels(TrnG))), TstX=NULL,var.equal=F){
  
  if(is.factor(TrnG)==F){
    mx <- nrow(TrnX)
    mg <- nrow(TrnG)
    TrnX <- rbind(TrnX,TrnG)
    TrnG <- factor(rep(1:2,c(mx,mg)))
  }
  
  if(is.null(TstX)==T) TstX <- TrnX
  if(is.vector(TstX)==T) TstX <- t(as.matrix(TstX))
  else if(is.matrix(TrnX)!=T) TstX <- as.matrix(TstX)
  
  if(is.matrix(TrnX)!=T) TrnX <- as.matrix(TrnX)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0,nx),nrow=1,dimnames=list("blong",1:nx))
  
  g <- length(levels(TrnG))
  mu <- matrix(0,nrow=g,ncol=ncol(TrnX))
  
  for(i in 1:g){
    mu[i,] <- colMeans(TrnX[TrnG==i,])
  }
  
  D <- matrix(0,nrow=g,ncol=nx)
  
  if(var.equal==T){
    for(i in 1:g){
      d2 <- mahalanobis(TstX, mu[i,],var(TrnX))
      D[i,] <- d2-2*log(p[i])
    }   
  }
  else{
    for(i in 1:g){
      S <- var(TrnX[TrnG==i,])
      d2 <- mahalanobis(TstX, mu[i,],S)
      D[i,] <- d2-2*log(p[i])-log(det(S))
    }
  }
  
  for(j in 1:nx){
    dmin <- Inf
    for(i in 1:g){
      if(D[i,j]<dmin){
        dmin <- D[i,j]
        blong[j] <- i
      }
    }
  }
  return(blong)  
}