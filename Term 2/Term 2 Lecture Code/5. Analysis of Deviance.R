#Hospital stay data
data(hosp, package="npmlreg")

#Full model
fit1 <- glm(duration~age+temp1+wbc1+antib+bact+serv, data=hosp,
            family=Gamma(link=log))

summary(fit1)

#Dispersion
summary(fit1)$dispersion

#Deviance table
anova(fit1)
anova(fit1, test="Chisq")

qchisq(0.95,1)
1-pchisq(1.103,1)
