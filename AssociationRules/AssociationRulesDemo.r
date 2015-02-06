library("arules")

data(Groceries)
summary(Groceries)

itemFrequencyPlot(Groceries, topN=20, names = T, main="Top 20")
itemFrequencyPlot(Groceries, support=0.1, names = T,main="Support 10%")

image(sample(Groceries,100))

rules <- apriori(Groceries, parameter=list(support=0.006,confidence=0.25))
inspect(rules[1:5])
inspect(sort(rules, by="lift")[1:10])

berryrules <- subset(rules, items %in% "berries")
inspect(berryrules)

df.rules <- as(rules, "data.frame")
setwd("D:/Dropbox/TEMP")
#write.table(df.rules, file="groceryrules.txt", sep=",", row.names=F)
#write.csv(df.rules, file="groceryrules.csv")
  