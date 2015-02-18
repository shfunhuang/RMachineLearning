# recommenderlab: A Framework for Developing and Testing Recommendation Algorithms

# Abstract
# This paper describes recommenderlab which provides the infrastructure to develop 
# and test recommender algorithms for rating data and 0-1 data in a unified framework.

# Introduction
# Recommender systems apply statistical and knowledge discovery techniques to the 
# problem of making product recommendations based on previously recorded data.
# Content-based approaches recommender can elicit the preference structure of a 
# customer(user) concerning product(item) attributes then we can recommend items which rank high for the user's most desirable attributes.
# Comtent-based recommendations based on system of hundreds of attributes to describe the essence of music at the fundamental level.

# Collaborative Filtering
# Collaborative filtering, the idea is thatgiven rating data by many users for many items,
# one can predict a user's rating for an item not known to him/her or create for a user a so called top-N lists of recommended items.
# The premise is that users who agreed on the rating for some items typically also agree on the rating for other items.

# Collaborative filtering(CF) users given rating data by many users for many items as the basis 
# for predicting missing rating and/or for creating a top-N recommendation list for given users, called active user.

# Creating a top-N list can be seen as a second step after predicting ratings for all 
# unknown items in Ia and then taking the N items with the highest predicted ratings.
# top-N list contains only the items with the highest estimated rating.

# Memory-based CF use the whole(or at least a large sample of the) user database to create recommendations.
# Model-based CF use the user database to learn a more compact model that is later used to create recommendations.

# User-based CF is a memory-based algorithm which tries to mimics word-of-mouth by analyzing rating data from many individuals.
# The assumption is that users with similar preferences will rate items similarly.
# Thus missing rating for a user can be predicted by first find the a neighborhood of similar users 
# and then aggregate the ratings of these users to form a prediction.
# The neighborhood is defined in terms of similarity between users, either by taking a given number of most similar user (k nearest neighbors)
# or all users within a given similarity threshold like Pearson correlation coefficient and the Cosine similarity.
# Now the neighborhood for the active user can be selected by either a threshold on the similarity or by taking k nearest neighbors.
# We compute the average ratings in the neighborhood for each item not rated by the active user.\

# Removing user rating bias by normalization.
# Normalization is used to remove individual rating bias by users who consistently always use lower or higher ratings than other users.

# Item-based CF is a model-based approach which produces recommendations based on the relationship between items inferred from the rating matrix.
# The assumption behind this approach is that users will prefer items that are similar to other items they like.
# The model-building step consists of calculating a similarity matrix containing all item-to-item similarities using a given similarity measure.
# User-bias can be reduced by first normalizing the user-item rating matrix before computing the item-to-item similarity matrix.

# If we only have information about which pages were viewed but not why some pages were not viewed.
# This situation leads to binary data or more exactly to 0-1 data  where 
# 1 means that we inferred that the user has a preference for an item
# 0 means that either the user does not like the item or does not know about it.

# Recommendations for 0-1 data based on Association Rules
# The binary profile matrix is seen as a database where each user is treated as a transaction that contains the subset of items with a rating of 1.

# Evaluation of Recommender Algorithms
# If a recommender algorithm performed better in predicting the withheld items, 
# it will also perform better in finding good recommendations for unknown items
# Mean Average Error(MAE)
# Root Mean Square Error(RMSE) penalizes larger errors stronger than MAE 
# and thus is suitable for situations where small prediction errors are not very important.

# Evaluation Top-N recommendations
# The confusion matrix shows how many of the items recommended in the top-N lists.
# A common error measure is the mean absolute error(MAE)
# Precision and recall are the best known measures used in information retrieval.
# F-measure is defined as the harmonic mean of precision and recall.
# A possible way to compare the efficiency of two systems is by comparing the size of the area under the ROC-curve,
# where a bigger area indicates better performance.

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



