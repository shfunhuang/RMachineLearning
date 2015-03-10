# �����ǲߤ����ۦ��׶q (Google)
# http://ppt.cc/9SHO (Reference)
x <- rnorm(10,1,2)
y <- rnorm(10,2,3)
xy <- rbind(x,y)

# 1.�ڤ�Z��(Euclidean Distance)
dist(xy, method="euclidean")
sqrt(sum((x-y)^2))
sqrt(t(x-y) %*% (x-y))

# 2.�ҫ��y�Z��(Manhattan Distance)
dist(xy, method="manhattan")
sum(abs(x-y))

# 3.���񳷤ҶZ��(Chebyshev Distance)
max(abs(x-y))
(sum(abs(x-y)^(dim(xy)[2])))^(1/dim(xy)[2])

# 4.�{�i�Ҵ���Z��(Minkowski Distance)
dist(xy, method="minkowski")
(sum((x-y)^dim(xy)[1]))^(1/dim(xy)[1])

# 5.�зǤƼڤ�Z��(Standardized Euclidean distance)
sum(((x-y)/apply(xy,2,sd))^2)

# 6.����Z��(Mahalanobis Distance)
sqrt(t(x-y) %*% solve(cov(xy)) %*% (x-y))  #singular?

# 7.�����l��(Cosine)
sum(x*y)/(sqrt(sum(x^2)) * sqrt(sum(y^2)))

# 8.�~���Z��(Hamming distance)
a <- rbinom(10,1,0.5)
b <- rbinom(10,1,0.7)
sum(abs(a-b))

# 9.�ǥd�w�ۦ��Y��(Jaccard similarity coefficient)
library("sets")
A <- set("a", "b", "c")
B <- set("c", "d", "e")
set_similarity(A, B)
set_dissimilarity(A, B) # �ǥd�w�Z��(Jaccard distance)

# 10.�����Y��(Correlation coefficient)�P�����Z��(Correlation distance)
cor(x,y)
cov(x,y)/(sd(x)*sd(y))

# 11.�H���i(Information Entropy)
library("entropy")
y <- c(4, 2, 3, 1, 2, 4, 3, 2, 2, 1, 1)
entropy(y, method="ML")


