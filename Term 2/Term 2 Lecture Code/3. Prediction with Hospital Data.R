#Hospital wait data
library(npmlreg)
data(hosp)

#Fitting GLM
hosp.glm <- glm(duration~age+temp1, data=hosp, family=Gamma(link=log))
summary(hosp.glm)

#Prediction age 60, temp 90
predict(hosp.glm, newdata=data.frame(age=60, temp1=99), type='response')

#95% confidence interval
#Predicted linear predictor
lphat <- predict(hosp.glm, newdata=data.frame(age=60, temp1=99))
#Extract covariance
varhat <- summary(hosp.glm)$cov.scaled
#Define new data point
x0 <- c(1,60,99)
#Compute width of interval for linear predictor
span <- qnorm(0.975)*sqrt(x0 %*% varhat %*% x0)
#Interval for the mean
c(exp(lphat-span), exp(lphat+span))
