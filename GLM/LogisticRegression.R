rm(list=ls())

### LOGISTIC REGRESSION TUTORIALS ###

### Logistic Regression: One Numerical Predictor

library("MASS")
data(menarche)
str(menarche)
summary(menarche)

# From the graph at right, it appears a logistic fit is called for here
plot(Menarche/Total ~ Age, data=menarche)

# With binomial errors in the response variable, the ordinary assumptions of least squares linear regression 
# (normality and homoscedasticity) don't apply.
glm.out <- glm(cbind(Menarche, Total-Menarche) ~ Age, family=binomial(link=logit), data=menarche)

# Look at how closely the fitted values from our logistic regression match the observed values
plot(Menarche/Total ~ Age, data=menarche)
lines(menarche$Age, glm.out$fitted, type="l", col="red")
title(main="Menarche Data with Fitted Logistic Regression Line")

# The response variable is log odds
# Coefficient of "Age" can be interpreted as 
# For every one year increase in age the odds of having reached menarche increase by exp(1.632) = 5.11 times.
summary(glm.out)

# To evaluate the overall performance of the model
anova(glm.out)

# "NULL DEVIANCE" shows how well the response is predicted by a model with nothing but an intercept (grand mean)
# This is essentially a chi square value on 24 degrees of freedom, and indicates very little fit (a highly significant difference between fitted values and observed values)

# The "RESIDUAL DEVIANCE" is 26.7 on 23 degrees of freedom.
1 - pchisq(26.7, df=23)

# We use this to test the overall fit of the model by once again treating this as a chi square value.
# The null hypothesis (i.e., the model) is not rejected. 
# The fitted values are not significantly different from the observed values.



### Logistic Regression: Multiple Numerical Predictors

file = "http://ww2.coastal.edu/kingw/statistics/R-tutorials/text/gorilla.csv"
gorilla <- read.csv(file) 
str(gorilla)

### Begin copying here.
gorilla <- data.frame(rep(c(0,1),c(30,19)),
                      c(126,118,61,69,57,78,114,81,73,93,116,156,90,120,99,113,103,123,
                        86,99,102,120,128,100,95,80,98,111,101,102,100,112,82,72,72,
                        89,108,88,116,100,99,93,100,110,100,106,115,120,97),
                      c(86,76,66,48,59,64,61,85,57,50,92,70,66,73,68,110,78,61,65,
                        77,77,74,100,89,61,55,92,90,85,78,66,78,84,63,65,71,46,70,
                        83,69,70,63,93,76,83,71,112,87,82),
                      c(64,54,44,32,42,53,41,47,33,45,49,45,48,49,44,47,52,28,42,51,54,
                        53,56,56,37,36,51,52,45,51,48,55,37,46,47,49,29,49,67,39,43,36,
                        62,56,36,49,66,54,41))
colnames(gorilla) <- c("seen","W","C","CW")
str(gorilla)
### End copying here.

### Correlation Matrix
cor(gorilla)

with(gorilla, tapply(W, seen, mean))
with(gorilla, tapply(C, seen, mean))
with(gorilla, tapply(CW, seen, mean))

glm.out <- glm(seen ~ W * C * CW, family=binomial(logit), data=gorilla)

# The first gives us what amount to regression coefficients with standard errors and a z-test, as we saw in the single variable example above.
summary(glm.out)

# The second print out shows the same overall reduction in deviance, from 65.438 to 57.281 on 7 degrees of freedom
anova(glm.out, test="Chisq")
1 - pchisq(65.438-57.281, df=48-41)

# Of note is the three-way interaction term, which produced a nearly significant reduction in deviance of 3.305 on 1 degree of freedom (p = 0.069).
1 - pchisq(60.586-57.281, df=42-41)

# Overall, the model appears to have performed poorly, showing no significant reduction in deviance (no significant difference from the null model).

plot(glm.out$fitted)
abline(v=30.5, col="red")
abline(h=0.3, col="green")
abline(h=0.5, col="green")
text(15, 0.9, "seen = 0")
text(40, 0.9, "seen = 1")



### Logistic Regression: Categorical Predictors

# The data are from 1973 and show admissions by gender to the top six grad programs at the University of California,
# Berkeley. Looked at as a two-way table, there appears to be a bias against admitting women...
ftable(UCBAdmissions, col.vars="Admit")

dimnames(UCBAdmissions)

# However, there are also relationships between "Gender" and "Dept" as well as between "Dept" and "Admit"
# margin.table(UCBAdmissions, c(2,3)) # "Gender" and "Dept"
# margin.table(UCBAdmissions, c(3,1)) # "Dept" and "Admit"
margin.table(UCBAdmissions, c(2,1)) # "Gender" and "Admit"

ucb.df <- data.frame(gender=rep(c("Male","Female"), c(6,6)),
                     dept=rep(LETTERS[1:6], 2),
                     yes=c(512,353,120,138,53,22,89,17,202,131,94,24),
                     no=c(313,207,205,279,138,351,19,8,391,244,299,317))
ucb.df

mod.form <- "cbind(yes,no) ~ gender * dept"
glm.out <- glm(mod.form, family=binomial(logit), data=ucb.df)

options(show.signif.stars=F)

# This is a saturated model, meaning we have used up all our degrees of freedom, and there is no residual deviance left over at the end.
# Saturated models always fit the data perfectly.
# In this case, it appears the saturated model is required to explain the data adequately.
# It appears all three terms are making a significant contribution to the model.
anova(glm.out, test="Chisq")

# These are the regression coefficients for each predictor in the mode
summary(glm.out)

# log odds
# The odds of male being admitted were only 0.35 times the odds of a female being admitted. 
# This shows that men were actually at a significant disadvantage when department and the interaction are controlled.
exp(-1.0521) # antilog of the genderMale coefficient

# The odds of female being admitted were 2.86 times the odds of a male being admitted.
1/exp(-1.0521)

# The odds of being admitted to dept C were only about 1/9th the odds of being admitted to dept A.
exp(-2.2046)

# department C to department D,
exp(-2.2046) / exp(-2.1662)          # C:A / D:A leaves C:D

# Statistics are nice, but in the end it's what makes sense that should rule the day.




