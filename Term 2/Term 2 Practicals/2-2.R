install.packages('readr')
library(readr)

#Load the data
footballdata <- read.csv("FootballData.csv")
View(footballdata)

#Matrices of indicator functions for home and away
X_home <- model.matrix(~0+HomeTeam, data=footballdata)
X_away <- model.matrix(~0+AwayTeam, data=footballdata)

#Want +1 if at home, -1 if away, 0 if not playing
X <- as.data.frame(X_home-X_away)
names(X) <- substring(names(X), 9)
library(stringr)
names(X) <- str_replace_all(names(X), c(" " = ".", "," = ""))
X <- X[,-1] #drop first team since we used indicator variables for factors

#Vector of home team wins (1) and draws or home losses(0) and make dataframe
y <- ifelse(footballdata$FTR == "H", 1, 0)
matchdata <- cbind(y = y, X)

head(matchdata)

#Fit logistic regression
fit1 <- glm(y ~ 0+., matchdata, family = binomial(link=logit))
summary(fit1)

#Sorting coefficients
sort(coef(fit1), decreasing=TRUE)

#For logistic regression, we expect dispersion parameter to be 1
fit1$deviance / fit1$df.residual # = 1.218547

#A home team advantage could be modelled as a constant multiplicative effect
#on the odds of a home team win. This is exactly the effect an intercept term
#would have.
#So, if we drop the 0+ to allow R to put in an intercept:
fit2 <- glm(y ~ ., matchdata, family = binomial(link=logit))
summary(fit2)
#Intercept is negative, but there's lots of uncertainty as small amount
#of data
confint(fit2, "(Intercept)")

#Plot of 95% CR for strengths of Man City and Liverpool using Hessian
# First, get the full Fisher information matrix
Finv_betahat <- summary(fit2)$cov.scaled
# For this confidence region, we just want the submatrix involving 
# Man City and Liverpool.
# Also, we want the inverse for the Mahalanobis distance
F_lmc <- solve(Finv_betahat[c(11, 13), c(11, 13)])
F_lmc

# Get the MLEs for these two teams
betahat_lmc <- coef(fit2)[c(11, 13)]

# Let's setup a grid of strengths which we'll check the Mahalanobis distance
# against the chi-squared critical value
liverpool <- seq(-4, 4, length.out = 300)

man_city <- seq(-4, 4, length.out = 300)

# Use the outer function to evaluate over a grid
HessCR <- outer(liverpool, man_city, 
                Vectorize(function(beta_l, beta_mc) {
                  beta <- c(beta_l, beta_mc)
                  t(betahat_lmc - beta) %*% F_lmc %*% (betahat_lmc - beta)
                }
                ))

# The image function now lets us colour the part we're interested in
image(liverpool, man_city, HessCR > qchisq(0.95, 2))
# and mark the location of the MLE for reference
points(betahat_lmc[1], betahat_lmc[2], pch = 3, col = "black")

#LR Test of model where beta Everton=Burnley=Sheff Utd against home
#advantage model
fit3 <- glm(y ~ . - Everton - Burnley - Sheffield.United
            + I(Everton + Burnley + Sheffield.United),
            matchdata, family = binomial(link=logit))
summary(fit3)

# In logistic regression, dispersion is 1, so the likelihood ratio test
# statistic can be found by just taking the difference of the deviances
fit3$deviance - fit2$deviance
# Difference in degrees of freedom (should be 2 as we've replaced 3 coefficients
# with 1)
fit3$df.residual - fit2$df.residual
# Test is against chi-sq(v=2)
qchisq(0.95, 2)
# LR is not larger, therefore not enough evidence to reject ... plausibly all
# teams up for relegation are equally weak

#Probability of a Chelsea win at Man City
# We need to predict a new match.  Easiest to pull a row from X, zero out and
# then set the home/away teams we need
new_match <- X[1,]
new_match[,1:19] <- 0
new_match$Man.City <- +1
new_match$Chelsea <- -1

View(new_match)

# Probability of Chelsea win is probability of away win, 
# so 1 minus predicted probability of home win
1-predict(fit3, new_match, type = "response")

#Use first 150 matches to train and then test the accuracy on testing data
# Split the data up
training <- matchdata[1:150,]
testing <- matchdata[-(1:150),]

# Fit the model on the training data only
fit4 <- glm(y ~ ., training, family = binomial(link=logit))
summary(fit4)

# Predict on the testing
pred <- predict(fit4, testing, type = "response")

# Now produce a table, where we want to compare the truth to our prediction
res <- table(truth = testing$y,
             prediction = ifelse(pred > 0.5, 1, 0))
res

# Hence overall accuracy (%) is
(res[1,1]+res[2,2])/sum(res)*100
