calib.data

#Scatterplot of fraction y/ncel vs dose with diameter of the plotted
#points proportional in size to ncel at each point
library(ggplot2)
ggplot(data=df) + 
  aes(x=calib.data$dose, y=calib.data$y/calib.data$ncel, size=calib.data$ncel) + 
  geom_point(alpha = 0.5, col = 'blue')

#Poisson model with log link
fit0 <- glm(y ~ dose + offset(log(ncel)), family=poisson(link="log"),
            data=calib.data)

#Linear and a quadratic dose term
fit1 <- glm(y ~ dose + I(dose^2) + offset(log(ncel)), family=poisson(link="log"),
            data=calib.data)
summary(fit1)

anova(fit1, test='Chisq')

#Estimating deviance
summary(fit1)$deviance/fit1$df.residual

1/fit1$df.residual * sum((calib.data$y-fit1$fitted)^2/fit1$fitted)

qchisq(0.95, fit1$df.residual)/fit1$df.residual

#Fit quasiPoisson model
fit2 <- glm(y ~ dose + I(dose^2) + offset(log(ncel)), family=quasipoisson(link="log"),
            data=calib.data)

#Fit with GEEs
library(gee)
fit3 <- gee(y ~ dose + I(dose^2) + offset(log(ncel)), family=poisson(link="log"),
            data=calib.data, id=rep(1,8), corstr="independence")

summary(fit3)$coefficients

ggplot(data=calib.data) + 
       aes(x=dose, y=y/ncel) + 
  geom_point(aes(size=ncel), alpha=0.8) + 
  geom_line(aes(x=dose, y=fit1$fitted/ncel), col=2, lwd=2)

#Given y/ncel = 0.2, what was the estimated dose?
B0 <- fit1$coef[1]
B1 <-fit1$coef[2]
B2 <- fit1$coef[3]

(-B1 + sqrt(B1^2 - 4*B2*(B0-log(0.2))))/B2

#Ungrouped data
dim(calib.raw.data)
head(calib.raw.data)

#Quasi-Poisson
fit4 <- glm(count ~ dose + I(dose^2), family=quasipoisson(link="log"),
            data=calib.raw.data)
summary(fit4)$coefficients

#Grouped Quasi-Poisson with identity link
fit5 <- glm(y ~ -1 + (ncel) + I(ncel*dose) + I(ncel*dose^2), 
            family=quasipoisson(link='identity'),
            data=calib.data)

summary(fit5)

#Add to plot
ggplot(data = calib.data, aes(x = dose, y = y)) +
  geom_point(aes(size=ncel), alpha = .8) +
  geom_line(aes(x=dose, y=fit1$fitted), col=2, lwd=2) +
  geom_line(aes(x=dose, y=fit5$fitted), col=3, lwd=2)

B0 <- fit5$coef[1]
B1 <-fit5$coef[2]
B2 <- fit5$coef[3]

(-0.2*B1 + sqrt((0.2*B1)^2 - 0.8*B2*(0.4*B0-0.04)))/0.4*B2
