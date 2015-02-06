# TrnX1 : train dataset of X1
# TrnX2 : train dataset of X2
# TstX : test dataset
discriminant.fisher <- function(TrnX1, TrnX2, TstX=NULL){
  
  if(is.null(TstX)==T) TstX <- rbind(TrnX1,TrnX2)
  
  if(is.vector(TstX)==T) TstX <- t(as.matrix(TstX))
  else if(is.matrix(TstX)!=T) TstX <- as.matrix(TstX)
  
  if(is.matrix(TrnX1)!=T) TrnX1 <- as.matrix(TrnX1)
  if(is.matrix(TrnX2)!=T) TrnX2 <- as.matrix(TrnX2)
  
  nx <- nrow(TstX)
  blong <- matrix(rep(0,nx),nrow=1,byrow=T,dimnames=list("blong",1:nx))
  
  n1 <- nrow(TrnX1)
  n2 <- nrow(TrnX2)
  
  mu1 <- colMeans(TrnX1)
  mu2 <- colMeans(TrnX2)
  
  S <- (n1-1)*var(TrnX1) + (n2-1)*var(TrnX2)
  mu <- n1/(n1+n2)*mu1 + n2/(n1+n2)*mu2
  w <- (TstX-rep(1,nx) %o% mu) %*% solve(S, mu2-mu1)
  
  for(i in 1:nx){
    if(w[i] <= 0) blong[i] <- 1
    else blong[i] <- 2
  }
  return(blong) 
}