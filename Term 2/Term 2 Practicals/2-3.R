install.packages('mlbench')
library(mlbench)
library(corrplot)

data("BostonHousing")
head(BostonHousing)

#Convert variable chas from binary to numeric
BostonHousing$chas <- as.numeric(BostonHousing$chas)

#Plot correlation matrix
cor_mat <- cor(BostonHousing) 
corrplot(cor_mat)

#Split data into test and train sets
set.seed(100)
indx <- sample(1:506, size=506, replace=F)
training_data <- BostonHousing[indx[1:400],]
test_data <- BostonHousing[indx[401:506],]

#Linear Regression model
fit1 <- lm(medv~., data=training_data)
summary(fit1)
#Insignificant variables: indus, age
fit1predict <- predict.lm(fit1, newdata=test_data)
mse1 <- mean((fit1predict - test_data$medv)^2)

#Linear regression via GLM with Gaussian and identity link
fit2 <- glm(medv~., data=training_data, family=gaussian())
summary(fit2)

fit2predict <- predict.lm(fit2, newdata=test_data, type="response")

mse2 <- sum((fit2predict - test_data$medv)^2)/105
#MSE are the same for both

fit2$deviance
sum((fit1$residuals)^2)
#Deviance = residual sum of squares

#Gamma GLM natural link
fit3 <- glm(medv~., data=training_data, family=Gamma(link='identity'))
fit3predict <- predict.glm(fit3, newdata=test_data, type="response")
mse3 <- sum((fit3predict - test_data$medv)^2)/105
#Smaller mse

#Gamma GLM log link
fit4 <- glm(medv~., data=training_data, family=Gamma(link='log'))
fit4predict <- predict.glm(fit4, newdata=test_data, type="response")
mse4 <- sum((fit4predict - test_data$medv)^2)/105
#MSE much higher now

#Calculate dispersion estimate from Pearson statistic
fit4.disp <- 1/(fit4$df.res) * sum((fit4$fitted-training_data$medv)^2/(fit4$fitted^2))
summary(fit4)

#Test between full model and model without indus and age
#Remove and add "indus" and "age" to move them to the last positions 
#in the independent variable order
model5 <- glm(medv ~ . -indus - age + indus + age, 
              data=training_data, family=Gamma(link=log))
summary(model5)

model5.anova <- anova(model5)
model5.anova_dev <- model5.anova$"Resid. Dev"

#Compute the p-value
1 - pchisq((model5.anova_dev[12]-model5.anova_dev[14])/fit4.disp, 2)

#Can't reject the reduced model