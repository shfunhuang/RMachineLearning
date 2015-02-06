library(psych)
student <- data.frame(
  x1=c(148,139,160,149,159,142,153,150,151,139,
       140,161,158,140,139,152,149,145,160,156,
       151,147,157,147,157,151,144,141,139,148),
  x2=c(41,34,49,36,45,31,43,43,42,31,
       29,47,49,33,31,35,47,35,47,44,
       42,38,39,30,48,36,36,30,32,38),
  x3=c(72,71,77,67,80,66,76,77,77,68,
       64,78,78,67,66,73,82,70,74,78,
       73,73,68,68,80,74,68,67,68,70),
  x4=c(78,76,86,79,86,76,83,79,80,74,
       74,84,83,77,73,79,79,77,87,85,
       82,78,80,75,88,80,76,76,73,78)  
  )


student.pr <- princomp(student, cor=T)
#student.pr <- princomp(~x1+x2+x3+x4, cor=T, data=student)
summary(student.pr, loading=T)
predict(student.pr)

screeplot(student.pr, type="l")
plot(student.pr)

biplot(student.pr)

### PCA??‰ç”¨
### PCA??†é??
load <- loadings(student.pr)
#load = matrix(c(-0.498,0.524,0.462,0.512,-0.513,-0.201,0.451,-0.701,-0.481,-0.733,-0.181,0.446,-0.506,0.384,-0.741,-0.215),4,4,byrow=T)
#??›é?…æ?‡æ??
plot(load[,1:2])
text(load[,1], load[,2], adj=c(-0.4,0.3))

### PCA Regression
### ??‹æ?å…±ç·šæ€§ç?„å?é??
economy <- data.frame(
  x1=c(149.3, 161.2, 171.5, 171.5, 180.8, 190.7,
       202.1, 212.4, 226.1, 231.9, 239.0),
  x2=c(4.2, 4.1, 3.1, 3.1, 1.1, 2.2, 2.1, 5.6, 5.0, 5.1, 0.7),
  x3=c(108.1, 114.8, 123.2, 126.9, 132.1, 137.7, 146.0, 154.1, 162.3, 164.3, 167.6), 
  y=c(15.9, 16.4, 19.0, 19.1, 18.8, 20.4, 22.7, 26.5, 28.1, 27.6, 26.3)
  
  )

lm.sol <- lm(y~x1+x2+x3, data=economy)
summary(lm.sol)

pairs.panels(economy[,1:3])

economy.pr <- princomp(~x1+x2+x3, data=economy, cor=T)
summary(economy.pr, loadings=T)

pre <- predict(economy.pr)
economy$z1 <- pre[,1]
economy$z2 <- pre[,2]

lm.sol <- lm(y~z1+z2, data=economy)
summary(lm.sol)



### è½‰æ?›æ?å?Ÿè?Šé??
beta <- coef(lm.sol)
A <- loadings(economy.pr)
x.bar <- economy.pr$center
x.sd <- economy.pr$scale
coef <- (beta[2]*A[,1] + beta[3]*A[,2])/x.sd
beta0 <- beta[1] - sum(x.bar * coef)

c(beta0, coef)




