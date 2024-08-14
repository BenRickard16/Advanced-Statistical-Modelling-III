#Hospital stay data
library(npmlreg)
data(hosp)
hosp.glm <- glm(duration~age+temp1, data=hosp, family=Gamma(link=log))

summary(hosp.glm)

#Calculating estimate dispersion by hand
1/(hosp.glm$df.res)*sum( (hosp$duration-hosp.glm$fitted)^2/(hosp.glm$fitted^2))
