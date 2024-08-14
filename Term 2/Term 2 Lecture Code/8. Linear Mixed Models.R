#Random Intercept Models
#Oxford boys data
require(nlme)
require(ggplot2)
ggplot(data = Oxboys, aes(x = age, y = height, col = Subject)) +
  geom_line(linewidth = 1.2, alpha = .8) +
  labs(title = "Height vs. Age", subtitle="by subject") +
  theme(legend.position = "none")

#Option1: Fit traditional regression model with as many levels
#as subjects
oxboys.int.lm <- lm(height~age+Subject, data=Oxboys)
oxboys.int.lm$coef
oxboys.int.pred <- predict(oxboys.int.lm)

ggplot(data = Oxboys, aes(x = age, y = height)) +
  geom_point(aes(col=Subject)) +
  geom_line(aes(y=oxboys.int.pred, col=Subject)) +
  labs(title = "Height vs. Age", subtitle="with subject-specific intercepts") +
  theme(legend.position = "none")

#Option2: Consider the subject-specific intercepts to be drawn 
#from a distribution centered at the overall intercept
require(lme4)
oxboys.lmm <- lmer(height ~ age + (1 | Subject), data=Oxboys)
oxboys.lmm

oxboys.lmm.pred <- predict(oxboys.lmm)  # predict xi^T beta + zi_T u_i
# will study later how u_i are predicted!

oxboys.lmm.marg <- predict(oxboys.lmm, re.form=NA) # predict xi^T beta
# corresponds to predicting the marginal model fit

ggplot(data = Oxboys, aes(x = age, y = height)) +
  geom_point(aes(col=Subject)) +
  geom_line(aes(y=oxboys.lmm.pred, col=Subject)) +
  geom_line(aes(y=oxboys.lmm.marg), lwd=3, colour=2) +
  labs(title = "Height vs. Age", subtitle="with subject-specific intercepts")+
  theme(legend.position = "none")

#Compute intra-class correlation for data set
#Firstly 'conditional'
oxboys.lmm

summary(oxboys.lmm)
oxboys.v <- as.data.frame(summary(oxboys.lmm)$varcor)
oxboys.v

icc <- oxboys.v[1,4]/(oxboys.v[1,4]+oxboys.v[2,4])
icc

#Then 'unconditional'
oxboys.int_only.lmm <- lmer(height ~ (1 | Subject), data=Oxboys)
oxboys.int_only.lmm

oxboys.int_only.v <- as.data.frame(summary(oxboys.int_only.lmm)$varcor)
icc <- oxboys.int_only.v[1,4]/(oxboys.int_only.v[1,4]+oxboys.int_only.v[2,4])
icc

#Automated
#install.packages('performance')
require(performance)
icc(oxboys.lmm)
icc(oxboys.int_only.lmm)

#Random Slope Models
require(lme4)
oxboys.slope.lmm <- lmer(height ~ age + (age | Subject), data=Oxboys)
oxboys.slope.lmm

oxboys.slope.lmm.pred <- predict(oxboys.slope.lmm)
oxboys.slope.lmm.marg <- predict(oxboys.slope.lmm, re.form=NA)

ggplot(data = Oxboys, aes(x = age, y = height)) +
  geom_point(aes(col=Subject)) +
  geom_line(aes(y=oxboys.slope.lmm.pred,  col=Subject)) +
  geom_line(aes(y=oxboys.slope.lmm.marg), lwd=3, colour=2) +
  labs(title = "Height vs. Age", subtitle="with subject-specific intercepts and slopes") +
  theme(legend.position = "none")

#REML and ML estimates for Oxford boys data
oxboys.slope.lmm <- lmer(height ~ age + (age | Subject), data=Oxboys)
oxboys.slope.lmm

oxboys.slope.lmm.ml <- lmer(height ~ age + (age | Subject), data=Oxboys, REML=FALSE)
oxboys.slope.lmm.ml

#REML and ML estimates for maths achievement data
sub_hsb
school.id <- as.factor(sub_hsb$schid)
hsb.lmm <- lmer(mathach~ses + (1|school.id), data=sub_hsb)
hsb.lmm

hsb.lmm.ml <- lmer(mathach~ses + (1|school.id), data=sub_hsb, REML=FALSE)
hsb.lmm.ml

summary(hsb.lmm)

#The t-value for the fixed effect slope ses is given by  
2.1202 / 0.2536 # = 8.359
#This is much bigger than 2 and hence is significantly different from 0 at 5% significance

#95% CI fo fixed effect component ses:
CI <- 2.1202 + qnorm(0.975)*c(-1,1)*0.2536
CI

#Interested in testing H_0: 'no linear trend for ses' vs H_1: 'linear trend for ses'
hsb.flat.lmm <- lmer(mathach~ 1 + (1|school.id), data=sub_hsb)
anova(hsb.flat.lmm, hsb.lmm)

confint(hsb.lmm)

#Comparison of lmm to gee
require(gee)
hsb.gee <- gee(mathach~ses, data=sub_hsb, id=school.id, corstr="exchangeable")

summary(hsb.gee)$coef
summary(hsb.lmm)$coef

#Prediction of random effects
#Extracting random effects predicted via BLUP
hsb.ran <- ranef(hsb.lmm)
hsb.ran

plot(hsb.ran)

#For random slope model
hsb.slope.lmm <- lmer(mathach~ses+ (ses|school.id), data=sub_hsb)
hsb.slope.lmm

plot(ranef(hsb.slope.lmm))

#Random intercept model
hsb.lmm
#Testing if the one random effect can be removed
install.packages('lmerTest')
require(lmerTest)

ranova(hsb.lmm)

#Same with random slope model
hsb.slope.lmm
ranova(hsb.slope.lmm)
