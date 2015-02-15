# recommenderlab: A Framework for Developing and Testing Recommendation Algorithms

rm(list=ls())
library("recommenderlab")
m <- matrix(sample(c(as.numeric(0:5), NA), 50, 
                   replace=TRUE, prob=c(rep(.4/6,6),.6)), ncol=10, 
            dimnames=list(user=paste("u", 1:5, sep=''), 
                          item=paste("i", 1:10, sep='')))
m
r <- as(m, "realRatingMatrix")
r
identical(as(r, "matrix"),m)

as(r, "list")
head(as(r, "data.frame"))
r_m <- normalize(r)
r_m

image(r, main = "Raw Ratings")
image(r_m, main = "Normalized Ratings")

r_b <- binarize(r, minRating=4)
as(r_b, "matrix")

data(Jester5k)
Jester5k

r <- sample(Jester5k, 1000)
r

rowCounts(r[1,])
as(r[1,], "list")

rowMeans(r[1,])
hist(getRatings(r), breaks=100)
hist(getRatings(normalize(r)), breaks=100)
hist(getRatings(normalize(r, method="Z-score")), breaks=100)
hist(rowCounts(r), breaks=50)
hist(colMeans(r), breaks=20)
recommenderRegistry$get_entries(dataType = "realRatingMatrix")

r <- Recommender(Jester5k[1:1000], method = "POPULAR")
r
names(getModel(r))
getModel(r)$topN

recom <- predict(r, Jester5k[1001:1002], n=5)
recom

as(recom, "list")

recom3 <- bestN(recom, n = 3)
recom3

as(recom3, "list")

recom <- predict(r, Jester5k[1001:1002], type="ratings")
recom

as(recom, "matrix")[,1:10]

# Evaluation of predicted ratings
e <- evaluationScheme(Jester5k[1:1000], method="split", train=0.9, given=15, goodRating=5)
e

r1 <- Recommender(getData(e, "train"), "UBCF")
r2 <- Recommender(getData(e, "train"), "IBCF")

p1 <- predict(r1, getData(e, "known"), type="ratings")
p2 <- predict(r2, getData(e, "known"), type="ratings")

error <- rbind(calcPredictionAccuracy(p1, getData(e, "unknown")), calcPredictionAccuracy(p2, getData(e, "unknown")))
rownames(error) <- c("UBCF","IBCF")
error

# Evaluation of a top-N recommender algorithm
scheme <- evaluationScheme(Jester5k[1:1000], method="cross", k=4, given=3, goodRating=5)
scheme

results <- evaluate(scheme, method="POPULAR", n=c(1,3,5,10,15,20))
results

getConfusionMatrix(results)[[1]]

avg(results)

plot(results, annotate=TRUE)
plot(results, "prec/rec", annotate=TRUE)

# Comparing recommender algorithms
scheme <- evaluationScheme(Jester5k[1:1000], method="split", train = .9, k=1, given=20, goodRating=5)
scheme

algorithms <- list("random items" = list(name="RANDOM", param=NULL),
                   "popular items" = list(name="POPULAR", param=NULL),
                   "user-based CF" = list(name="UBCF", param=list(method="Cosine", nn=50, minRating=5)))
results <- evaluate(scheme, algorithms, n=c(1, 3, 5, 10, 15, 20))
results
names(results)
results[["user-based CF"]]

plot(results, annotate=c(1,3), legend="topleft")
plot(results, "prec/rec", annotate=3)

Jester_binary <- binarize(Jester5k, minRating=5)
Jester_binary <- Jester_binary[rowCounts(Jester_binary)>20]
Jester_binary

scheme_binary <- evaluationScheme(Jester_binary[1:1000], method="split", train=.9, k=1, given=20)
scheme_binary

algorithms_binary <- list(
  "random items" = list(name="RANDOM", param=NULL),
  "popular items" = list(name="POPULAR", param=NULL),
  "user-based CF" = list(name="UBCF", param=list(method="Jaccard", nn=50)))

results_binary <- evaluate(scheme_binary, algorithms_binary, n=c(1,3,5,10,15,20))

plot(results_binary, annotate=c(1,3), legend="bottomright")



