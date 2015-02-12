rm(list=ls())
library("arules")
library("arulesViz")

### Finding Patterns - Market Basket Analysis Using Association Rules
# Concept
# Methods for finding useful associations in large databases using simple statistical performance measures
# How to manage the peculiarities of working with transactional data
# The start-to-finish steps needed for using association rules to perform a market basket analysis on real-world data
# Market basket analysis is used behind the scenes for the recommendation systems used in many brick-and-mortar and on-line retailers.

# The Apriori algorithm for association rule learning
# The result of a market basket analysis is a set of association rules that specify patterns of relationships among items.
# Unsupervised knowledge discovery
# All subsets of a frequent item-set must also be frequent, in other words, if {A, B} is frequent, then {A} and {B} both must be frequent.

# Whether or not an association rule is deemed interesting is determined by two statistical measures: support and confidence.

# The SUPPORT of an item-set or rule measures how frequently it occurs in the data.
# Support(X) = count(X)/N
# Setting support is to think about the minimum number of transactions you would need before you would consider a pattern interesting.

# A rule's CONFIDENCE is a measurement of its predictive power or accuracy. 
# The confidence tells us the proportion of transactions where the presence of item or item-set X results in the presence of item or itemset Y.
# Confidence(X->Y) = support(X,Y)/support(X)
# Setting the minimum confidence involves a tricky balance.
# The appropriate minimum confidence level depends a great deal on the goals of your analysis.

# The LIFT is a metric we have not considered yet.
# It is a measure of how much more likely one item is to be purchased relative to its typical purchase rate, given that you know another item has been purchased.
# Lift(X->Y) = confidence(X->Y)/support(Y)
# If lift(X->Y) > 1, this implies that the two items are found together more often than one would expect by chance.
# A large lift value is therefore a strong indicator that a rule is important, and reflects a true connection between the items.

# the sparse matrix we created, 9835 rows refer to the store transactions, 
# and 169 columns are features for each of the 169 different items that might appear in someone's grocery basket.
# The density value of 0.02609146 (2.6 percent) refers to the proportion of non-zero matrix cells.
data(Groceries)
summary(Groceries)

# A histogram showing the eight items in the groceries data with at least 10 percent support:
itemFrequencyPlot(Groceries, support=0.1, names = T, main="Support 10%")

# The following diagram for the top 20 items in the groceries data:
itemFrequencyPlot(Groceries, topN=20, names = T, main="Top 20")

# Cells in the matrix are filled with black for transactions (rows) where the item (column) was purchased.
image(sample(Groceries,100))

# Support and confidence parameters to produce a reasonable number of association rules.
rules <- apriori(Groceries, parameter=list(support=0.006, confidence=0.25))

# To look at the contents of the sparse matrix, use the inspect() function in combination with vector operators.
inspect(rules[1:5])
inspect(sort(rules, by="lift")[1:10])

berryrules <- subset(rules, items %in% "berries")
inspect(berryrules)

df.rules <- as(rules, "data.frame")
#write.table(df.rules, file="groceryrules.txt", sep=",", row.names=F)

# for vis asrules
inspect(head(sort(rules, by ="lift"),3))
plot(rules)
head(quality(rules))
plot(rules, measure=c("support", "lift"), shading="confidence")
plot(rules, shading="order", control=list(main = "Two-key plot"))
sel <- plot(rules, measure=c("support", "lift"), shading="confidence", interactive=TRUE)

subrules <- rules[quality(rules)$confidence > 0.8]
subrules
plot(subrules, method="matrix", measure="lift")
plot(subrules, method="matrix", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix3D", measure="lift")
plot(subrules, method="matrix3D", measure="lift", control=list(reorder=TRUE))
plot(subrules, method="matrix", measure=c("lift", "confidence"))
plot(subrules, method="matrix", measure=c("lift", "confidence"), control=list(reorder=TRUE))
  
plot(rules, method="grouped")
plot(rules, method="grouped", control=list(k=50))

subrules2 <- head(sort(rules, by="lift"), 10)
plot(subrules2, method="graph")
plot(subrules2, method="graph", control=list(type="items"))

plot(subrules2, method="paracoord")
plot(subrules2, method="paracoord", control=list(reorder=TRUE))

oneRule <- sample(rules, 1)
inspect(oneRule)
plot(oneRule, method="doubledecker", data = Groceries)
