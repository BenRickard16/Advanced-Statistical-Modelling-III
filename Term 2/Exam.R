#Load in the necessary libraries
require("datasets")
require("ggplot2")
require("gee")

#Load in the dataset
data(UKLungDeaths)
ldeaths

#Creating a dataframe
total_deaths <- as.data.frame(matrix(c(1:length(ldeaths), t(ldeaths)), ncol=2))
colnames(total_deaths) <- c("time", "deaths")

#Plotting the data
ggplot(data=total_deaths, aes(x=1974+(time-1)/12, y=deaths)) +
  geom_segment(aes(x=1974+(time-1)/12, y=1000, xend=1974+(time-1)/12, yend=deaths))

plot(1974+(total_deaths$time-1)/12, total_deaths$deaths, type="h")

#Fit Poisson GLM and add fitted curve to the plot
fit0 <- glm(deaths~time, family=poisson(link=log), data=total_deaths)

ggplot(data=total_deaths, aes(x=1974+(time-1)/12, y=deaths)) +
  geom_segment(aes(x=1974+(time-1)/12, y=1000, xend=1974+(time-1)/12, yend=deaths)) + 
  geom_line(aes(x=1974+(time-1)/12, y=fit0$fitted, lwd=1, col='red'))

plot(1974+(total_deaths$time-1)/12, total_deaths$deaths, type="h")
lines(1974+(total_deaths$time-1)/12, fit0$fitted)

#Add the 2 annual components to the model
fit1 <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12)),
                 family=poisson(link=log), data=total_deaths)

#Add the 2 6-monthly components to the model
fit2 <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
            + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
            family=poisson(link=log), data=total_deaths)

summary(fit2)

#Add fitted curves from fit1 and fit2 to the plot
plot(1974+(total_deaths$time-1)/12, total_deaths$deaths, type="h")
lines(1974+(total_deaths$time-1)/12, fit0$fitted, col='green')
lines(1974+(total_deaths$time-1)/12, fit1$fitted, col='red')
lines(1974+(total_deaths$time-1)/12, fit2$fitted, col='blue')

#Dispersion of model fit2 by Pearson
disp <- 1/fit2$df.residual * sum(((total_deaths$deaths - fit2$fit)^2)/fit2$fit)

summary(fit2)

#Test whether fit1 or fit2 is better at 1% significance
fit2_anova <- anova(fit2)
fit2_anova_dev <- fit2_anova$"Resid. Dev"

1 - pchisq((fit2_anova_dev[4]-fit2_anova_dev[6])/disp, 2)

anova(fit1q,fit2q, test='Chisq')

#Fit models 1 and 2 with quasiPoisson and perform test to see which is preferred
fit1q <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12)),
            family=quasipoisson(link=log), data=total_deaths)

fit2q <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
            + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
            family=quasipoisson(link=log), data=total_deaths)
summary(fit2q)
summary(fit2)

fit2q_anova <- anova(fit2q)
fit2q_anova_dev <- fit2q_anova$"Resid. Dev"

1 - pchisq((fit2q_anova_dev[4]-fit2q_anova_dev[6])/disp, 2)

#Fit model 2 with a GEE AR-1 correlation structure each cluster has data points
#from within the same year
gee2 <- gee(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
            + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
            family=poisson(link=log), data=total_deaths, corstr='AR-M',
            id = rep(1:6, each=12), Mv=1)
gee2
summary(gee2)$working.correlation
0.0898962452^2

#Find inverse of fisher information matrix of fit2q
summary(fit2q)$cov.unscaled

#Predict deaths for 1980-1983 using fit2q
pred <- predict(fit2q, newdata=data.frame('time'=c(73:108)), type='response')
pred[1:6]

newtime=73:108

colours <- rep('red', 108)
colours[1:72] = 'black'

plot(x=append(1974+(total_deaths$time-1)/12, 1974+(newtime-1)/12) , y=append(total_deaths$deaths,pred), type="h",
     col=colours)

#95% CI for the the predicted points

time<-73
CIlow <- rep(0,36)
CIhigh <- rep(0,36)
while (time < 109){
  newdata <- c(1, time, cos(2*pi*time/12), sin(2*pi*time/12), cos(2*pi*time/6),sin(2*pi*time/6))
  span <- qnorm(0.975)*sqrt(t(newdata) %*% summary(fit2q)$cov.unscaled %*% newdata)
  fitted <- log(pred[time-72])
  CIlow[time-72]<- exp(fitted-span)
  CIhigh[time-72]<-exp(fitted+span)
  time=time+1
}

head(cbind(CIlow, CIhigh))


#Add previous deaths as a covariate to fit2
fit2 <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
            + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
            family=poisson(link=log), data=total_deaths)
total_deaths$previous_deaths <- rep(0, 72)

i=2
while (i <= 72){
  total_deaths$previous_deaths[i] <- total_deaths$deaths[i-1]
  i=i+1
}

total_deaths$previous_deaths[1] <- 2600
head(total_deaths)

fit2.1 <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
            + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6))+previous_deaths, 
            family=poisson(link=log), data=total_deaths)

anova(fit2.1, test='Chisq')

fit2.1q <- glm(deaths~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
             + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6))+previous_deaths, 
             family=quasipoisson(link=log), data=total_deaths)
anova(fit2.1q, test='Chisq')
