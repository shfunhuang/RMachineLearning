# 機器學習中的相似度量 (Google)
# http://ppt.cc/9SHO (Reference)
x <- rnorm(10,1,2)
y <- rnorm(10,2,3)
xy <- rbind(x,y)

# 1.歐氏距離(Euclidean Distance)
dist(xy, method="euclidean")
sqrt(sum((x-y)^2))
sqrt(t(x-y) %*% (x-y))

# 2.曼哈頓距離(Manhattan Distance)
dist(xy, method="manhattan")
sum(abs(x-y))

# 3.切比雪夫距離(Chebyshev Distance)
max(abs(x-y))
(sum(abs(x-y)^(dim(xy)[2])))^(1/dim(xy)[2])

# 4.閔可夫斯基距離(Minkowski Distance)
dist(xy, method="minkowski")
(sum((x-y)^dim(xy)[1]))^(1/dim(xy)[1])

# 5.標準化歐氏距離(Standardized Euclidean distance)
sum(((x-y)/apply(xy,2,sd))^2)

# 6.馬氏距離(Mahalanobis Distance)
sqrt(t(x-y) %*% solve(cov(xy)) %*% (x-y))  #singular?

# 7.夾角餘弦(Cosine)
sum(x*y)/(sqrt(sum(x^2)) * sqrt(sum(y^2)))

# 8.漢明距離(Hamming distance)
a <- rbinom(10,1,0.5)
b <- rbinom(10,1,0.7)
sum(abs(a-b))

# 9.傑卡德相似係數(Jaccard similarity coefficient)
library("sets")
A <- set("a", "b", "c")
B <- set("c", "d", "e")
set_similarity(A, B)
set_dissimilarity(A, B) # 傑卡德距離(Jaccard distance)

# 10.相關係數(Correlation coefficient)與相關距離(Correlation distance)
cor(x,y)
cov(x,y)/(sd(x)*sd(y))

# 11.信息熵(Information Entropy)
library("entropy")
y <- c(4, 2, 3, 1, 2, 4, 3, 2, 2, 1, 1)
entropy(y, method="ML")


