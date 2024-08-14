#8 repeated measures of 26 different boys
library(nlme)
data(Oxboys)
library(ggplot2)

#Plotting the data
ggplot(data=Oxboys, aes(x=age, y=height, col=Subject)) +
  geom_point(size=1.2, alpha=.8) + 
  labs(title= "Height vs. Age", subtitle="")

ggplot(data=Oxboys, aes(x=age, y=height, col=Subject)) +
  geom_line(linewidth=1.2, alpha=.8) +
  labs(title= "Height vs. Age", subtitle="by subject") +
  theme(legend.position = "none")

#Maths achievement data for 30 schools
#Covariates include socioeconomic status
head(sub_hsb)
dim(sub_hsb)

school.id <- as.factor(sub_hsb$schid)
length(levels(school.id))

#Plotting the data
ggplot(data = sub_hsb, aes(x = ses, y = mathach)) +
  geom_point(size = 0.8, alpha = .8) +
  geom_smooth(method = "lm", se = FALSE, col = "Red")+
  ggtitle("Mathematics achievement vs. Socioeconomic status (SES)") +
  xlab("SES") +
  ylab("Mathematics achievement")

ggplot(data = sub_hsb, aes(x = ses, y = mathach, colour = school.id)) +
  geom_point(size = 0.8, alpha = .8) +
  geom_smooth(method="lm", se=FALSE) +
  ggtitle("Mathematics achievement vs. Socioeconomic status (SES)",
          subtitle ="by cluster (school)") +
  xlab("SES") +
  ylab("Mathematics achievement") +
  theme(legend.position = "none")

#Fitting the GEE
require(gee)
hsb.gee <- gee(mathach ~ ses, data=sub_hsb, id=school.id, 
               corstr="exchangeable")
hsb.gee

#Compare to linear regression
hsb.lm <- lm(mathach ~ ses, data=sub_hsb)

sub_hsb$pred1 <- predict(hsb.gee)

ggplot(data = sub_hsb, aes(x = ses, y = mathach)) +
  geom_point(size = 0.8, alpha = .8) +
  geom_smooth(method = "lm", se = FALSE, col = "Red") +
  geom_line(aes(x = ses, y = pred1), col = "Blue") +
  ggtitle("Mathematics achievement vs. Socioeconomic status (SES)",
          subtitle = "Blue line is GEE solution") +
  xlab("SES") +
  ylab("Mathematics achievement")

#What are standard errors?
summary(hsb.gee)$coef
summary(hsb.lm)$coef
#Estimates and SEs differ

#GEE for Oxford boys height data
data(Oxboys)
oxboys.gee <- gee(height ~ age, data=Oxboys, id=Subject, 
                  corstr="AR-M", Mv=1)
oxboys.gee

#Compare to linear regression
oxboys.lm <- lm(height~age, data=Oxboys)
oxboys.lm

summary(oxboys.gee)$coef
summary(oxboys.lm)$coef
#SEs under GEE decrease