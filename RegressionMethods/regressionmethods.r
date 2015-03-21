### Forecasting Numeric Data - Regression Methods
# These methods can be used for forecasting numeric data and quantifying 
# the size and strength of a relationship between an OUTCOME and its PREDICTIONS. 
# How to use hybrid models known as REGRESSION TREE and MODEL TREE, which allow decision trees to be used for numeric prediction.

# A single value numeric Dependent Variable(the value to be predicted).
# One or more numeric Independent Variables(the predictors).
# Regression equations model data using a similar slope-intercept format.
# Examining how populations and individuals vary by their measured characteristics.
# Quantifying the causal relationship between an event and the response.
# Regression methods are also used for HYPOTHESIS testing, 
# which involves determining whether data indicate that a presupposition is more likely to be TRUE or FALSE.

# Logistic Regression can be used to model a binary categorical outcome
# Poisson Regression modeling integer count data.
# Linear models are generalized via the use of a link function, which specifies the mathematical relationship between x and y.

# Regression analysis involves finding PARAMETER ESTIMATES for alpha and beta.
# In order to determine the optimal estimates alpha and beta, an estimation methods known as ordinary least squares(OLS).
# Minimize the sum of the squared errors(Residuals).

# The Correlation between two variables is a number that indicates how closely their relationship follows a straight line.
# Correlation does NOT imply causation.

# Find values of beta coefficients that minimize the prediction error of a linear equation.
# y changes by the amount  beta_i, for each unit increase in x_i.
# The intercept is then the expected value of y when the independent variables are all zero.

### Collecting Data
# https://github.com/stedy/Machine-Learning-with-R-datasets

insurance <- read.csv("insurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$charges)

# Linear regression assumes a NORMAL distribution for the dependent variable(y)
# Regression models require that every feature is numeric.
hist(insurance$charges)
table(insurance$region)

# Before fitting a regression model to data, it can be useful to determine how the independent variables are related to the dependent variable and each other.
# A correlation matrix provides a quick overview of these relationships.
cor(insurance[c("age", "bmi", "children", "charges")])

# Visualizing relationships among features - the scatter-plot matrix
library("psych")
pairs(insurance[c("age", "bmi", "children", "charges")])
pairs.panels(insurance[c("age", "bmi", "children", "charges")])

# Training a model on the data
ins_model <- lm(charges ~ age + children + bmi + sex + smoker + region, data = insurance)
ins_model <- lm(charges ~ ., data = insurance)

# The intercept tells us the value of charges when the independent variables are equal to zero. 
ins_model

# The estimated beta coefficients indicate the increase in charges for an increase of one in each of the features are hold constant.
# Dummy Coding to each of the factor type variables we included in the model.
# Dummy Coding allows a NORMINAL feature to be treated as numeric by creating a BINARY variable for each category of the feature.
# One category is always left out to serve as the reference category.

# Evaluating model performance
# Residuals: Error in our predictions
# 50% of errors fall within the 1Q and 3Q values.

# The stars: provides a measure of how likely the true coefficient is zero given the value of the estimate.
# which means that the feature is extremely unlikely to be unrelated to the dependent variable.

# The Multiple R-squared: a measure of how well our model as a whole explains the values of the dependent variables.
# The closer the value is to 1.0, the better the model perfectly explains the data.
# Because models with more features always explain more variation, 
# the Adjusted R-squared value corrects R-squared by penalizing models with a large number of independent variables.
summary(ins_model)

# Improving model performance
# Regression typically leaves FEATURE SELECTION and model specification to the user.
insurance$age2 <- insurance$age^2
insurance$bmi30 <- ifelse(insurance$bmi >= 30, 1, 0)

# Model specification - adding interaction effects
ins_model2 <- lm(charges ~ age + age2 + children + bmi + sex + bmi30*smoker + region, data = insurance)
summary(ins_model2)

# Understanding Regression Trees and Model Trees
# Regression Trees, predictions based on the average value of examples that reach a leaf.
# Model Trees, multiple linear regression model is built from the examples reaching that node.

# Decision trees may be better suited for tasks with many features or many complex, non-linear relationships among features and the outcome.
# The data are partitioned using a divide-and-conquer strategy according to the feature 
# that will result in the greatest increase in homogeneity in the outcome after split is performed.

# Splitting Criterion: Standard Deviation Reduction(SDR)

# Collecting data - UCI Machine Learning Data Repository
wine <- read.csv("whitewines.csv")
str(wine)

# One of the advantages of trees is that they can handle many types of data without preprocessing.
# This means we do NOT need to normalize or standardize the features.
hist(wine$quality)

# Most wines are of average quality; few are particularly bad or good.
wine_train <- wine[1:3750, ]
wine_test <- wine[3751:4898, ]

# Training a model on the data
# Perform regression tree modeling, the rpart(recursive partitioning)
# The rpart function can fit classification trees or regression trees.
library("rpart")
m.rpart <- rpart(quality ~ ., data = wine_train)
m.rpart

# Visualizing decision tress
library("rpart.plot")
rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type = 3, extra = 101)

# Evaluating model performance
p.rpart <- predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)
cor(p.rpart, wine_test$quality)

# Measuring performance with Mean Absolute Error(MAE)
# Mean of the absolute value of errors.
MAE <- function(actual, predicted) {
	mean(abs(actual - predicted))
}
MAE(p.rpart, wine_test$quality)

mean(wine_train$quality) #5.870933
MAE(5.87, wine_test$quality)

# Improving model performance
# Model tree improves on regression trees by replacing the leaf nodes with regression models.
library("RWeka")
m.m5p <- M5P(quality ~ ., data = wine_train)
m.m5p
summary(m.m5p)

# The values can be interpreted exactly the same as the multiple regression models we built earlier in this chapter.
# Total of 36 linear models were built in this model tree, each with different estimations of the impact of fixed acidity and the 10 other features.
p.m5p <- predict(m.m5p, wine_test)
summary(p.m5p)

cor(p.m5p, wine_test$quality)
MAE(wine_test$quality, p.m5p)

# Linear regression, involves fitting straight lines to data.
# Regression trees, which use the average value of examples at leaf nodes to make numeric predictions.
# Model trees, which build a regression model at each leaf node in a hybrid approach that is in some ways the best of both worlds.
