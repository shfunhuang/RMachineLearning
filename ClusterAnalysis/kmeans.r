### Finding Group of Data - Clustering with k-means
# There types of stereotypes are dangerous to apply to individuals - no two people are exactly alike.
# Used in aggregrate, however, the labels may reflect some underlying pattern of similarity among the individuals falling within the group.

# Clustering is an unsupervised machine learning task that automatically divides the data into clusters, or groupings of similar items.
# It does this without having been told what the groups shoud look like ahead of time.
# Clustering is used for KNOWLEDGE DISCOVERY rather than prediction.
# It provides an insight into the natural groupings found within data.

# Clustering is guided by the principle that records inside a cluster should be very similar to each other, butvery different from those outside.
# It results in meaningful and actionable structures within data that reduce complexity and provide insight into patterns of relationships.

# Clustering creates new data.
# Unlabeled examples are given a cluster label and inferred entirely from the relationships within the data.
# The clustering task referred to as UNSUPERVISED CLASSIFICATION because, in a sense, this is classifying unlabeled examples.

# Yhe catch is that the class labels obtained from an unsupervised classifer are without intrinsic meaning.
# The cluster labels in uncertain term.
# Using a measure of how closely the examples are related, they can be assigned to homogeneous groups.

# Heuristic Process
# This means that it starts with an initial guess for the cluster assignments then modifies the assignments slightly to see if the changes improve the homogeneity within the clusters.
# It updates the assignments by adjusting the cluster boundaries according to the examples that currently fall into the cluster.
# The process of updating and assigning occurs several times until making changes no longer improves the cluster fit.
# You may end up with somewhat different final results by making only slight changes to the start conditions.

# K-means treats feature values as coordinates in a multi-dimensional feature space.
# We are using distance calculations, all data need to be NUMERIC, and the values should be NORMALIZED to a standard range ahead of time.

# The algorithm can be sensitive to RANDOMLY chosen cluster centers.
# You will have some PRIOR KNOWLEDGE about the true groupings, and you can begin applying k-means using this information.
# Sometimes the number of clusters is dictated by business requirements or the motivation for the analysis.
# Without any PRIOR KNOWLEDGE at all, one rule of thumb suggests setting k equal to the square root of (n/2).

# Elbow Method
# The homogeneity within clusters is expected to increase as additional clusters are added.
# The heterogeneity will also continue to decrease with more clusters.
# There are numerous statistics to measure homogeneity and heterogeneity within clusters that can be used with the elbow method.
# Clustering is unsupervised, the task is really about what you make of it - the insights you take away from the algorithm's findings.

# Social Networking Service(SNS) data
# https://github.com/stedy/Machine-Learning-with-R-datasets
teens <- read.csv("snsdata.csv")

# The record has a missing value
# For nominal values
table(teens$gender, useNA = "ifany")

# For numerical values
summary(teens$age)

# The minimum and maximun values seem to be the suspect.
teens$age <- ifelse(teens$age >= 13 & teens$age < 20, teens$age, NA)
summary(teens$age)

# An alternative solution for CATEGORICAL data like gender is to treat a missing value as a separate categpry.
# Dummy coding involves creating a separate binary 1 or 0 valued dummy variable for each level of a nominal feature except one,
# which is held out to serve as the reference group.
teens$female <- ifelse(teens$gender = "F" & !is.na(teens$gender), 1, 0)
teens$no_gender <- ifelse(is.na(teens$gender), 1 ,0)

# NUMERIC data
# Imputation method, which involves filling in the missing data with a guess as to what the true value really is.
mean(teens$age, na.rm = TRUE)

# We need the average age for each graduation year.
# The aggregate() function is a tool for computes statistics for subgroups of data.
aggregate(data = teens, age ~ gradyear, mean, na.rm = TRUE)

# The ave() function, which return a vector with the group means repeated such that the result is equal in length to the original vector
ave_age <- ave(teens$age, teens$gradyear, FUN = function(x) mean(x, na.rm = TRUE))
teens$age <- ifelse(is.na(teens$age), ave_age, teens$age)
summary(teens$age)

# The kmeans() function requires a data frame containing only numeric data and a parameter specifying the desired number of clusters.
interests <- teens[5:40]

# NORMALIZE or Z-SCORE STANDARDIZE
# If the z-score value is 3, we know that that they mentioned football many more times than the average teenager.
interests_z <- as.data.frame(lapply(interests, scale))

# If we use too many clusters, we may find them too specific to be useful; conversely,
# Choosing too few may result in heterogeneous groupings.

# Five Stereotype: a Brain, an Athlete, a Basket Case, a Princess and a Criminal.
teen_clusters <- kmeans(interests_z, 5)

# The success or failure of the model hinges on whether the clusters are useful for their intended purpose.
# One of the most basic ways to evaluate the utility of a set of clusters is to examine the number of examples falling in each of the groups.
teen_clusters$size

# For a more in-depth look at the clusters, we can examine the coordinates of the cluster centroids.
teen_clusters$centers

# The output values indicate the average value for the interest listed at the top of the column.
# As the values are Z-SCORE standardized, negative values are below the overall mean for all studends and positive values are above the mean.

# Because clustering creates new information,
# The performance of a clustering algorithm depends at least somewhat on both the quality of the clusters themselves as well as what you do with that information.

# NOVEL INSIGHTS
teens$cluster <- teen_clusters$cluster
teens[1:10, c("cluster", "gender", "age", "friends")]

# DEMOGRAPHIC CHARACTERISTICS
aggregate(data = teens, age ~ cluster, mean)

# Notable difference in the proportion of females by cluster
aggregate(data = teens, female ~ cluster, mean)

aggregate(data = teens, friends ~ cluster, mean)

# The association among group membership, gender, and number of friends suggests that the clusters can be useful predictors.
# In order to obtain the highest degree of performance,
# It is CRUCIAL to be able to define and MEASURE it in the strictest terms.
