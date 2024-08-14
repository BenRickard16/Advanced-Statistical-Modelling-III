#Hospital stay data
require(npmlreg)
data(hosp)
plot(hosp[,c("duration","age","temp1","wbc1")])

#Fit the GLM
hosp.glm<-glm(duration~age+temp1, data=hosp, family=Gamma(link=log))
summary(hosp.glm)

#Dispersion estimate (Pearson)
hosp.disp <- 1/(hosp.glm$df.res)*sum((hosp$duration-hosp.glm$fitted)^2/
                                       (hosp.glm$fitted)^2)

#Dispersion estimate (Deviance)
hosp.glm$deviance / hosp.glm$df.residual

#Calculating standard errors of calculated coeffs
summary(hosp.glm)$cov.unscaled #F^(-1) under phi=1

sqrt(hosp.disp)*sqrt(diag(summary(hosp.glm)$cov.unscaled))
summary(hosp.glm)$coef[,"Std. Error"]

#Checking for overdispersion
#Recall Poisson model has disperion phi=1
require(gamlss.data)
data(polio)
uspolio <- as.data.frame(matrix(c(1:168, t(polio)), ncol=2))
colnames(uspolio) <- c("time", "cases")
plot(uspolio, type="h")

polio.glm <- glm(cases ~ time, family=poisson(link=log), data=uspolio)
summary(polio.glm)

#Dispersion estimate
polio.disp <- 333.55/166
polio.disp

#Now seasonal model
polio2.glm <- glm(cases ~ time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                  + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                  family=poisson(link=log), data=uspolio)
summary(polio2.glm)

polio2.disp <- 288.65/162
polio2.disp

#Dispersion decreases when improving the fit of the model

#Quasi-likelihood method
#Pearson dispersion estimates
polio.phi <- 1/(polio.glm$df.res) * sum((uspolio$cases-polio.glm$fitted)^2/(polio.glm$fitted))
polio.phi

polio2.phi <- 1/(polio2.glm$df.res) * sum((uspolio$cases-polio2.glm$fitted)^2/(polio2.glm$fitted))
polio2.phi

#Adjust the standard errors
polio.se <- sqrt(polio.phi)*summary(polio.glm)$coef[,2]
polio.se

polio2.se <- sqrt(polio2.phi)*summary(polio2.glm)$coef[,2]
polio2.se 

#Or can be obtained directly
polio.qglm <- glm(cases ~ time, family=quasipoisson(link=log), data=uspolio)
summary(polio.qglm)

polio2.qglm <- glm(cases ~ time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                   + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                   family=quasipoisson(link=log), data=uspolio)
summary(polio2.qglm)

#Testing for overdispersion at 5%
#Critical values for H_0: phi=1
qchisq(0.95, polio.glm$df.res)/polio.glm$df.res
qchisq(0.95, polio2.glm$df.res)/polio2.glm$df.res
#Hence reject H_0 in both cases

#First we use GEEs to provide an equivalent estimate of the 
#quasipoisson approach
require(gee)

uspolio.gee <- gee(cases ~ time 
                   + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                   + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                   family=poisson(link=log), id=rep(1,168),
                   corstr = "independence", data=uspolio)
#Running glm to get initial regression estimate

uspolio.gee
summary(uspolio.gee)$coef

#But there is likely serial coorelation so
uspolio.gee2 <- gee(cases ~ time 
                   + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                   + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                   family=poisson(link=log), id=rep(1,168),
                   corstr = "AR-M", data=uspolio, Mv=1)
#This doesn't fit... Try another package
require(geepack)
uspolio.gee3 <- geeglm(cases ~ time 
                       + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12)) 
                       + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                       family=poisson(link=log), id=rep(1,168), 
                       corstr="ar1", data=uspolio)

uspolio.gee3

#Can simplify the model by assuming AR(1) correlation structure
#within each year, but different years are independent
require(gee)
id = rep(1:14, each=12)
id

uspolio.gee4 <- gee(cases ~ time 
                    + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                    + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)),
                    family=poisson(link=log), id=id, 
                    data=uspolio, corstr="AR-M", Mv=1)
uspolio.gee4

round(summary(uspolio.gee4)$working.correlation, digits=3)
#The assumption on the correlation structure of the data leads us
#to repeated measures data which is the actual strenngth of GEEs