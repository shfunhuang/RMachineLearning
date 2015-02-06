### S:sample variance 
### R:similarity matrix
### m:# of main factors
factor.analysis <- function(S, m=0, d=1/diag(solve(S)),method="likelihood"){

  if(m==0){
    p <- nrow(X)
    eig <- eigen(S)
    sum_eig <- sum(diag(S))
    
    for(i in 1:p){
      if(sum(eig$values[1:i])/sum_eig>0.70){
        m <- i
        break
      }
    }
  }
  source("factor.analysis1.r")
  source("factor.analysis2.r")
  source("factor.analysis3.r")
  switch(method,
         princomp=factor.analysis1(S, m),
         factor=factor.analysis2(S, m, d),
         likelihood=factor.analysis3(S, m, d)
         )
}

