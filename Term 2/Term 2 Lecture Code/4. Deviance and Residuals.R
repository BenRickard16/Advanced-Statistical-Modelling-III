#Polio cases data
library(gamlss.data)
data(polio)
uspolio <- as.data.frame(matrix(c(1:168, t(polio)), ncol=2))
colnames(uspolio) <- c("time", "cases")

head(uspolio)
plot(uspolio, type='h')

#Poisson GLM with time, 6 and 12 month cycles
polio2.glm<- glm(cases~time + I(cos(2*pi*time/12))+I(sin(2*pi*time/12))
                 + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                 family=poisson(link=log), data=uspolio)
summary(polio2.glm)

plot(1970 + ((uspolio$time-1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time-1)/12), polio2.glm$fitted,col=4)

#Deviance
polio2.glm$dev

#Pearson Statistic
sum((uspolio$cases-polio2.glm$fitted)^2/polio2.glm$fitted)

#Critical value at 5% df = 168-6
qchisq(0.95, 162)
#So reject H_0

#Adding scaled temperature data
temp_data <- rep(c(5.195, 5.138, 5.316, 5.242, 5.094, 5.108, 5.260, 5.153, 
                   5.155, 5.231, 5.234, 5.142, 5.173, 5.167), each = 12 )
scaled_temp = 10 * (temp_data - min(temp_data))/(max(temp_data) - min(temp_data))
uspolio$temp = scaled_temp

# Construct GLM
polio3.glm <- glm(cases~time + temp 
                  + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                  + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
                  family=poisson(link=log),data=uspolio)
summary(polio3.glm)

plot(1970 + ((uspolio$time-1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time-1)/12), polio3.glm$fitted, col="red")

#Deviance
polio3.glm$dev

#Pearson statistic
sum((uspolio$cases-polio3.glm$fitted)^2/polio3.glm$fitted)

#Critical value at 5% df=168-7
qchisq(0.95, 161)
#So still reject H_0, although deviance and Pearson statistic
#both decreased


#Hospital stay data
data(hosp, package="npmlreg")
hosp.glm <- glm(duration~age+temp1, data=hosp, family=Gamma(link=log))

par(mfrow=c(2,2))
plot(residuals(hosp.glm, type="deviance"))
plot(hosp.glm$fitted, residuals(hosp.glm, type="pearson"))
plot(hosp$age, residuals(hosp.glm, type="deviance"))
plot(hosp$temp1, residuals(hosp.glm, type="deviance"))
#No obvious patterns but sample size is quite small

#Check autocorrelations
cor(residuals(hosp.glm, type="deviance")[1:24], residuals(hosp.glm, type="deviance")[2:25])
cor(residuals(hosp.glm, type="pearson")[1:24], residuals(hosp.glm, type="pearson")[2:25])
#Some positive autocorrelations, but the sample size is again
#quite small

#For Polio case data
#Polio2 model
par(mfrow=c(2,1))
plot(uspolio$time, uspolio$cases, type="h")
lines(uspolio$time, polio2.glm$fitted, col="blue")
plot(uspolio$time, residuals(polio2.glm, type="deviance"), type="b")
abline(a=0,b=0)

#Polio3 model
plot(uspolio$time, uspolio$cases, type="h")
lines(uspolio$time, polio3.glm$fitted, col="red")
plot(uspolio$time, residuals(polio3.glm, type="deviance"), type="b")
abline(a=0,b=0)

#Here there is clearly autocorrelation present, so independence
#of different y_i is violated

#Computing autocorrelations
cor(residuals(polio2.glm, type="deviance")[2:168], residuals(polio2.glm, type="deviance")[1:167])
cor(residuals(polio3.glm, type="deviance")[2:168], residuals(polio3.glm, type="deviance")[1:167])
#Reduced in second model, but still high