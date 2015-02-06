# TrnX1 : train dataset of X1
# TrnX2 : train dataset of X2
# TstX : test data
discriminant.bayes <- function(TrnX1, TrnX2, TstX=NULL, rate=1, var.equal=F){
  
  if(is.null(TstX)==T) TstX <- rbind(TrnX1,TrnX2)
  if(is.null(TstX)==T) TstX <- t(as.matrix(TstX))
  else if(is.matrix(TstX)!=T) TstX <- as.matrix(TstX)
  
  if(is.matrix(TrnX1)!=T) TrnX1 <- as.matrix(TrnX1)
  if(is.matrix(TrnX2)!=T) TrnX2 <- as.matrix(TrnX2)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0,nx),nrow=1,byrow=T,dimnames=list("blong",1:nx))
  mu1 <- colMeans(TrnX1)
  mu2 <- colMeans(TrnX2)
  
  if(var.equal==T){
    S <- var(rbind(TrnX1,TrnX2))
    beta <- 2*log(rate)
    w <- mahalanobis(TstX,mu2,S)-mahalanobis(TstX,mu1,S)
  }
  else{
    S1 <- var(TrnX1)
    S2 <- var(TrnX2)
    beta <- 2*log(rate)+log(det(S1)/det(S2))
    w <- mahalanobis(TstX,mu2,S2)-mahalanobis(TstX,mu1,S1)
  }
  
  for(i in 1:nx){
    if(w[i]>beta)
      blong[i] <- 1
    else
      blong[i] <- 2
  }
  return(blong)
}