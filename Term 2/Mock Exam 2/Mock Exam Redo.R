#Plot scatterplot of y/ncel against dose
library(ggplot2)
ggplot(data=calib.data, aes(x=dose, y/ncel)) + 
  geom_point(aes(size=ncel))

#Fit Poisson model with log link
fit0 <- glm(y ~ dose + offset(log(ncel)), family=poisson(link = "log"), 
            data=calib.data)

#Fit with linear and quadratic dose term and perform a goodness of fit test
fit1 <- glm(y ~ dose + I(dose^2) + offset(log(ncel)), data=calib.data, 
            family=poisson(link='log'))

anova(fit0, fit1, test="Chisq")
#From the table, p value when adding the quadratic dose term is less than 
#2.2e-16. So, fit1 is better than fit0

#Estimation of dispersion by fit1 by deviance and by Pearson
summary(fit1)$deviance/fit1$df.res

1/fit1$df.res * sum((fit1$fit-calib.data$y)^2/fit1$fit)

#Pearson-based dispersion estimate to formally test for overdispersion
#Give the critical value of that test when using a 5% level of significance
qchisq(0.95, fit1$df.res) / fit1$df.res

#Fit a quasi-Poisson model, and then an equivalent model with GEEs
fit1q <- glm(y ~ dose + I(dose^2) + offset(log(ncel)), 
             family=quasipoisson(link = "log"), data=calib.data)
summary(fit1q)$coef

require(gee)
fit1gee <- gee(y ~ dose + I(dose^2) + offset(log(ncel)), 
               corstr="independence", id=rep(1,8), 
               family=poisson(link = "log"), data=calib.data)
summary(fit1gee)$coef

#Add fitted curve from fit1 to the scatterplot
ggplot(data=calib.data, aes(x=dose, y/ncel)) + 
  geom_point(aes(size=ncel)) + 
  geom_line(aes(x=dose, y=fit1$fitted/ncel), col=2, lwd=2)

#Fit quasiPoisson model with the raw data
fit1q.raw <- glm(count ~ dose + I(dose^2), 
                 family=quasipoisson(link = "log"), data=calib.raw.data)
summary(fit1q.raw)$coef

#Fit quasiPoisson model for grouped data with identity link
fit2q <- glm(y ~ -1 + I(ncel) + I(ncel*dose) + I(ncel*dose^2), 
             family=quasipoisson(link = "identity"), data=calib.data)
summary(fit2q)

#Add this fitted curve to the plot
ggplot(data=calib.data, aes(x=dose, y=y/ncel)) + 
  geom_point(aes(size=ncel)) + 
  geom_line(aes(x=dose, y=fit1$fitted/ncel), col=2, lwd=2) + 
  geom_line(aes(x=dose, y=fit2q$fitted/ncel), col=3, lwd=2)
