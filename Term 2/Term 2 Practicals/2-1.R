#install.packages("npmlreg")
library("npmlreg")
data(irlsuicide)

#Add death rates
irlsuicide$rate <- irlsuicide$death / irlsuicide$pop
irlsuicide$notdead = irlsuicide$pop - irlsuicide$death
names(irlsuicide)

#EDA - Boxplots
boxplot(rate~ID, data=irlsuicide)
boxplot(rate~age, data=irlsuicide)
boxplot(rate~sex, data=irlsuicide)
plot(irlsuicide[c("rate", "age", "sex", "ID")])

#Using proportions and weights
glm1 <- glm(rate~age+sex, weights=pop, family=binomial(link=logit),
    data=irlsuicide)
summary(glm1)

# Using numbers of each category
glm2 = glm(cbind(death, notdead)~age+sex, data = irlsuicide,
           family = binomial()) #logit link is default
summary(glm2)

#Including regional indicators
glm3 <- glm(rate~age+sex+ID, weights=pop, data=irlsuicide, 
            family=binomial())
summary(glm3)
#ID 4 and 7 least significant, all others are significant

#Using region predictor instead of ID
glm4 <- glm(rate~age+sex+Region, weights=pop, data=irlsuicide, 
            family=binomial())
summary(glm4)

#Preciction male from Galway age 40-59
predict(glm3, newdata=data.frame(sex='1', ID='3', age='3'), type="response")

#Fitting Poisson model
glm5 <- glm(death~age+sex, family=poisson(link=log),
            data=irlsuicide, offset=log(pop)) #log link default Poisson
summary(glm5)

#Adding ID
glm6 <- glm(log(death)~age+sex+ID, family=poisson(),
            data=irlsuicide, offset=log(pop))
summary(glm6)

#Prediction again
deathhat = predict(glm6, newdata = data.frame(pop = 4989, ID = '3', age = '3', sex = '1'), type = "response")
(deathhat/4989)

#Estimating dispersion
( phiEst = (1/glm5$df.res) * sum(((irlsuicide$death - glm5$fitted)^2)/glm5$fitted) )

( phiEst = (1/glm6$df.res) * sum(((irlsuicide$death - glm6$fitted)^2)/glm6$fitted) )

#Plot observed rates and fittedd rates from binomial and poisson model
results = cbind(irlsuicide$rate, glm3$fitted, glm6$fitted)

results = cbind(irlsuicide$rate, glm3$fitted, glm6$fitted/irlsuicide$pop)

resultsd = data.frame(results)

plot(resultsd)
