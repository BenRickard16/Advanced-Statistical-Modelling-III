shuttle <- read.table("shuttle.asc", header=TRUE)
getwd()

#Linear model
fit1 <- lm(td~temp, data=shuttle)
plot(shuttle$temp,shuttle$td, ylab='Probability of Thermal Overstrain', xlab='Temperature')
abline(lm(td~temp, data=shuttle), col='red')
#Linear model not suitable as implies there are temperatures
#giving negative probabilities and probabilities greater
#than 1, which are both not legitimate

#Logistic regression model with logit link
shuttle.glm <- glm(td~temp, family=binomial(link=logit),data=shuttle)

( coefficients <- as.numeric(shuttle.glm$coef) )

#Functions giving probability of thermal overstrain given
#temperature
shuttle.fit <- function(temp){
  exp(coefficients[1] + coefficients[2] * temp) /
    ( 1 + exp(coefficients[1] + coefficients[2] * temp) )
}
shuttle.fit2 <- function(temp, model){
  coefficients <- as.numeric(model$coef)
  exp(coefficients[1] + coefficients[2] * temp) /
    ( 1 + exp(coefficients[1] + coefficients[2] * temp) )
}

#Adding the plot to graph
lines(30:90, shuttle.fit2(30:90, model = shuttle.glm), lty = 2)

#Finding the temp where prob thermal overstrain is 0.5
shuttle.fit.inverse <- function(prob, model){
  coefficients <- as.numeric(model$coef)
  (log( prob / ( 1- prob ) ) - coefficients[1]) / coefficients[2]
}
shuttle.fit.inverse(0.5, model = shuttle.glm )

#Probability of thermal overstrain given temperature 31
shuttle.fit2(31, model = shuttle.glm)

#Now with probit function
( shuttle.glm2 <- glm(td~temp, data=shuttle, family=binomial(link="probit")) )
shuttle.glm2$coef
shuttle.fit.probit <- function( temp, model ){
  coefficients <- as.numeric(model$coef)
  pnorm(coefficients[1] + coefficients[2] * temp)
}
shuttle.fit.probit.inverse <- function( prob, model ){
  coefficients <- as.numeric(model$coef)
  ( qnorm( prob ) - coefficients[1] ) / coefficients[2]
}
shuttle.fit.probit.inverse( 0.5, model = shuttle.glm2 )
shuttle.fit.probit( 31, model = shuttle.glm2 )


#US graduate school admissions data
graduate <- read.csv("binary.csv")
head(graduate)
summary(graduate)

#Logistic regression
#Tell R that rank is categorical not numerical
graduate$rank <- factor(graduate$rank)
graduate.glm <- glm(admit ~ gre + gpa + rank, data = graduate, 
                    family = "binomial")
summary(graduate.glm)

#To get OR
exp(coef(graduate.glm))

#Predict the probability of admission for a student with GRE 
#700, GPA 3.7, to a rank 2 institution
predData <- with(graduate, data.frame(gre = 700, gpa = 3.7, rank = factor(2)))
predData$prediction <- predict(graduate.glm, newdata = predData,
                               type = "response")

#Remove terms from model and check AIC
summary(graduate.glm)$aic
graduate.glm2 <- glm(admit ~ gre + gpa, data = graduate, 
                     family = "binomial")
summary(graduate.glm2)$aic
graduate.glm3 <- glm(admit ~ gre + rank, data = graduate, 
                     family = "binomial")
summary(graduate.glm3)$aic
graduate.glm4 <- glm(admit ~ gpa + rank, data = graduate, 
                     family = "binomial")
summary(graduate.glm4)$aic

#Probit link function
graduate.glm.probit <- glm(admit ~ gre + gpa + rank, data = graduate, 
                           family = "binomial"(link="probit"))
summary(graduate.glm.probit)

predData$prediction2 <- predict(graduate.glm.probit, newdata = predData, 
                                type = "response")
