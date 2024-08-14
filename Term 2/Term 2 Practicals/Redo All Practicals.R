#Data on number of suicides in Ireland
library(npmlreg)
data("irlsuicide")

#Calculating death rates
irlsuicide$rate <- irlsuicide$death / irlsuicide$pop

#Producing boxplots
boxplot(rate ~ age, data=irlsuicide)
boxplot(rate ~ sex, data=irlsuicide)
boxplot(rate ~ Region, data=irlsuicide)

plot(irlsuicide[c("rate", "age", "sex", "ID")])

#Model of suicide rate with Binomial logit model
glm1 <- glm(rate~age+sex, data=irlsuicide, weights=pop, family=binomial(link='logit'))
summary(glm1)

#Include ID as a covariate
glm2 <- glm(rate~age+sex+ID, weights=pop, data=irlsuicide, family=binomial())
summary(glm2)
#ID4 and ID11 are not significant

#Predict suicide prob. for a male resident of Galway age 40-59
predict(glm2, newdata=data.frame(ID='3', sex='1', age='3'), type='response')

#Fit a Poisson model
glm3 <- glm(death~age+sex, offset=log(pop), data=irlsuicide, family=poisson())
summary(glm3)

#With ID
glm4 <- glm(death~ID+age+sex, offset=log(pop), data=irlsuicide, family=poisson())
summary(glm4)

#Same prediction
deathhat <- predict(glm4, new=data.frame(ID='3', sex='1', age='3', pop=4989),
        type='response')

ratehat <- deathhat / 4989

#Dispersion estimates
1/glm3$df.resid * sum((irlsuicide$death - glm3$fitted)^2/glm3$fitted)
1/glm4$df.resid * sum((irlsuicide$death - glm4$fitted)^2/glm4$fitted)

#Matrix scatterplot which shows observed rates and 
results = cbind(irlsuicide$rate, glm2$fitted, glm4$fitted)
results = cbind(irlsuicide$rate, glm2$fitted, glm4$fitted/irlsuicide$pop)
resultsd = data.frame(results)
plot(resultsd)

#Is it okay to include a large number of regional indicators in these models?
#Risk of separation? Is region likely to influence suicide rate? Allows
#overfitting?
#In modelling regional effects through indicators, we neglect the (most
#probably present) correlation between neighbouring regions. Morevover,
#through the indicators, one introduces a very large number of parameter,
#which eats up degrees of freedom, and so increases the variance of our
#estimators.


#Premier league football scores data
#Load the data
Football <- read.csv("C:\\Users\\benan\\OneDrive\\Documents\\FootballData.csv")
View(Football)
#Preparing the design matrix
#Helpfully, R splits factors into columns, so we can use it to create an
#indicator variable for each team both when home and away
X_home <- model.matrix( ~ 0+HomeTeam, data=Football)
X_away <- model.matrix( ~ 0+AwayTeam, data=Football)

View(X_home)
View(X_away)

#X_home has 1 when home, and X_away has 1 when away, so we want the difference
#Convert the difference into a data frame so variables (columns) are named
X <- as.data.frame(X_home - X_away)

nrow(X)
ncol(X)

#Optionally, we can pretty up the names by removing the "HomeTeam" part 
#at the start
names(X) <- substring(names(X), 9)
#Use the "stringr" package to replace the empty space in team names with a dot.
library(stringr)
names(X) <- str_replace_all(names(X), c(" " = ".", "," = ""))

#To avoid a singular solution due to collinearity of indicators, we simply
#drop the first team, Arsenal
X <- X[,-1]

# Finally, create vector y of home team wins and make data frame
y <- ifelse(Football$FTR == "H", 1, 0)

matchdata <- cbind(y = y, X)
View(matchdata)

#Bradley-Terry model via a logistic regression
# Finally, create vector y of home team wins and make data frame
fit1 <- glm(y ~ ., data=matchdata, family=binomial(link='logit'))
summary(fit1)

#Expect the dispersion parameter to be 1
#Estimating from the model
fit1$deviance / fit1$df.residual
#Slight overdispersion

#Plot showing 95% CR for Man City and Liverpool by Hessian method
#First, get the full Fisher information matrix
Finv_betahat <- summary(fit1)$cov.scaled
#For this confidence region, we just want the submatrix involving 
#Man City and Liverpool.
#Also, we want the inverse for the Mahalanobis distance
F_lmc <- solve(Finv_betahat[c(11, 13), c(11, 13)])
F_lmc

#Get the MLEs for these two teams
betahat_lmc <- coef(fit1)[c(11, 13)]

#Let's setup a grid of strengths which we'll check the Mahalanobis distance
#against the chi-squared critical value
liverpool <- seq(-4, 4, length.out = 300)
man_city <- seq(-4, 4, length.out = 300)

#Use the outer function to evaluate over a grid
HessCR <- outer(liverpool, man_city, 
                Vectorize(function(beta_l, beta_mc) {
                  beta <- c(beta_l, beta_mc)
                  t(betahat_lmc - beta) %*% F_lmc %*% (betahat_lmc - beta)
                }
                ))

#The image function now lets us colour the part we're interested in
image(liverpool, man_city, HessCR > qchisq(0.95, 2))
#and mark the location of the MLE for reference
points(betahat_lmc[1], betahat_lmc[2], pch = 3, col = "black")

#You could also add the individual confidence intervals calculated by R as
#horizontal and vertical lines, but note it is doing something called profiling
#to get the confidence interval accounting for all other variables so you will
#notice a small discrepancy between the marginal and the extremes of the joint
abline(h = confint(fit1, "Man.City"))
abline(v = confint(fit1, "Liverpool"))

#Calculate the probability that current bottom 3 teams are relegated
#We need to compute the model with the three teams fixed to have the same
#coefficient. To do this, simply tell R to remove the individual predictors
#and insert a new predictor formed from all three
fit2 <- glm(y ~ . - Everton - Burnley - Sheffield.United
            + I(Everton + Burnley + Sheffield.United),
            matchdata, family = binomial(link=logit))
summary(fit2)

#In logistic regression, dispersion is 1, so the likelihood ratio test
#statistic can be found by just taking the difference of the deviances
fit2$deviance - fit1$deviance
#Difference in degrees of freedom (should be 2 as we've replaced 3 coefficients
#with 1)
fit2$df.residual - fit1$df.residual
#Test is against chi-sq(v=2)
qchisq(0.95, 2)
#LR is not larger, therefore not enough evidence to reject ... plausibly all
#teams up for relegation are equally weak

#Predict prob of Chelsea winning at Man City
#Easiest to pull a row from X, zero out and then set the home/away teams we need

new_match <- X[1,]
new_match[,1:19] <- 0
new_match$Man.City <- +1
new_match$Chelsea <- -1

View(new_match)

#Probability of Chelsea win is probability of away win, 
#so 1 minus predicted probability of home win
1-predict(fit1, new_match, type = "response")

#Calculate accuracy of prediction by splitting up train data into train and test
#Split the data up
training <- matchdata[1:150,]
testing <- matchdata[-(1:150),]

#Fit the model on the training data only
fit2 <- glm(y ~ ., data=training, family = binomial(link=logit))
summary(fit2)

#Predict on the testing
pred <- predict(fit2, testing, type = "response")

#Now produce a table, where we want to compare the truth to our prediction
res <- table(truth = testing$y,
             prediction = ifelse(pred > 0.5, 1, 0))
res

#Hence overall accuracy (%) is
(res[1,1]+res[2,2])/sum(res)*100

#Data of house values in Boston
library(mlbench)
data("BostonHousing")

#Summary of the dataset
str(BostonHousing)

#Plot the correlation matrix between all variables
library(corrplot)
BostonHousing$chas <- as.numeric(BostonHousing$chas)
cor_mat <- cor(BostonHousing)
corrplot(cor_mat)

#Randomly split dataset into training and testing sets
set.seed(1)
indx <- sample(1:506, size=506, replace=F)
BostonHousing.train <- BostonHousing[indx[1:400],]
BostonHousing.test <- BostonHousing[indx[401:506],]

#First fit a simple linear regression model
model1 <- lm(medv~., data=BostonHousing.train)
summary(model1)

#Predict responses of test set and calculate the MSE
pred1 <- predict(model1, newdata=BostonHousing.test)

mean((BostonHousing.test$medv-pred1)^2)

#Fit linear regression using GLM (Gaussian with identity)
model2 <- glm(medv~., data=BostonHousing.train, family=gaussian())
summary(model2)

#Compare the RSS model1 and deviance of model2
sum((model1$fit - BostonHousing.train$medv)^2)
model2$deviance

#Calculating the MSE
pred2 <- predict(model2, newdata=BostonHousing.test)
mean((BostonHousing.test$medv - pred2)^2)

#Now fit a Gamma GLM with natural link to training set and calculate MSE
model3 <- glm(medv~., data=BostonHousing.train, family=Gamma())
summary(model3)

pred3 <- predict(model3, newdata=BostonHousing.test)
mean((BostonHousing.test$medv - pred3)^2)

#Gamma GLM again with log link
model4 <- glm(medv~., data=BostonHousing.train, family=Gamma(link=log))
summary(model4)

pred4 <- predict(model4, newdata=BostonHousing.test)
mean((BostonHousing.test$medv - pred4)^2)

#Compute an estimation of dispersion by the Pearson statistic
disp <- 1/(model4$df.res)*sum((BostonHousing.train$medv-model4$fitted)^2/(model4$fitted^2))
disp

#Perform analysis of deviance test between full model and reduced model without
#indus and age at 5%
model5 <- glm(medv ~ . -indus - age + indus + age, 
              data=BostonHousing.train, family=Gamma(link=log))
summary(model5)

model5.anova <- anova(model5)
model5.anova_dev <- model5.anova$"Resid. Dev"               

1 - pchisq((model5.anova_dev[12]-model5.anova_dev[14])/disp, 2)


#Popularity data
install.packages('haven')
library(haven)
pop.rawdata <- read_sav(file="https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")

require(gee)
require(ggplot2)

#Creating a dataframe from first 6 columns
pop.data <- data.frame(pop.rawdata[,1:6])
head(pop.data)

#Changing the column names
colnames(pop.data) <- c('pupil','class', 'extraversion', 'gender', 'experience', 'popularity')

#Exploratory analysis
ggplot(data=pop.data, aes(popularity)) +
  geom_histogram(bins=15)
#Hence we can model the response as a Gaussian

#Viewing distribution of the predictor variables
ggplot(data=pop.data, aes(x=extraversion)) +
  geom_bar(stat="count")
ggplot(data=pop.data, aes(x=gender)) +
  geom_bar(stat="count")
ggplot(data=pop.data, aes(x=experience)) +
  geom_bar(stat="count")
#Shape of distribution of predictor variables not important from 
#statistical modelling point of view

#Plot pop. vs extraversion with simple linear regression line
ggplot(data=pop.data, aes(x=extraversion, y=popularity)) +
  geom_point(size=1, alpha=.8) +
  geom_smooth(method='lm', se=FALSE, col='red', linewidth=1, alpha=0.8)

#Class-specific linear regression lines
ggplot(data = pop.data,
       aes(x = extraversion, y = popularity, colour = as.factor(class))) +
  geom_jitter(size=0.8) + # to add some random noise for plotting purposes
  labs(title = "Popularity versus extraversion", subtitle = "by pupil") +
  theme(legend.position = "none") +
  geom_smooth(method='lm', se=FALSE)

#Modelling
#Simple linear regression
pop.simple <- lm(popularity~extraversion, data=pop.data)
summary(pop.simple)

#Fit marginal models for popularity vs extraversion with independent,
#exchangeable, and unstructured working correlation matrix
pop.gee0 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='independence')
pop.gee0
summary(pop.gee0)$coef
#Equivalent to the simple linear regression

pop.gee1 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='exchangeable')
pop.gee1 
summary(pop.gee1)$coef
#Parameter estimate changed a lot
#SE for intercept slightly increased, and for extraversion slightly decreased

pop.gee2 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='unstructured')
pop.gee2
summary(pop.gee2)$coef
#Unstructured and exchangeable models give similar parameter estimates
#SEs of latter model a bit smaller, and differences in correlation matrix are
#large

#Plot regression lines from exchangeable and unstructure
pred1 <- predict(pop.gee1)
frame1 <- unique(data.frame(extraversion=pop.data$extraversion, pred1=pred1))

pred2 <- predict(pop.gee2)
frame2 <- unique(data.frame(extraversion=pop.data$extraversion, pred2=pred2))


ggplot(data = pop.data, aes(x = extraversion, y = popularity)) +
  geom_point(size = 1,alpha = .8) +
  geom_smooth(method = "lm", # to add regression line
              col = "red", size = 1, alpha = .8, se=FALSE) +
  geom_line(data=frame1, aes(x=extraversion, y=pred1), col="Blue") +
  geom_line(data=frame2, aes(x=extraversion, y=pred2), col="Green")

#Now with all covariates
pop.gee0a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="independence", data=pop.data)
pop.gee0a
summary(pop.gee0a)$coef


pop.gee1a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="exchangeable", data=pop.data)
pop.gee1a
summary(pop.gee1a)$coef


pop.gee2a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="unstructured", data=pop.data)
pop.gee2a
summary(pop.gee2a)$coef