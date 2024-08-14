getwd()
ShakyBirds.rdata
install.packages("DAAG")
library("DAAG")


dim(ShakyBirds)
dimnames(ShakyBirds)

head(possum)
dim(possum)
dimnames(possum)
?possum


#Marginal table 
BirdsA <- margin.table(ShakyBirds,margin=c("Food", "Feet"))
BirdSums <- addmargins(BirdsA)

#Q2
BirdMarg <- prop.table(addmargins(prop.table( BirdsA, 2),2),2)

#Chi square independence
CST <- chisq.test(BirdsA, correct=FALSE)

CST$stdres
CST$residuals

prop.table(margin.table(ShakyBirds,c(1,2,4)))

library(vcd)
sieve(ShakyBirds,shade=T)

#marginalise over food
table1 <- margin.table(ShakyBirds,c(1,3,4))
table2 <- aperm(table1,c(1,3,2))
mantelhaen.test(table2, correct=FALSE, alternative="g")
#p-value = 0.006543
#strong evidence

#marginal over food and feet
table3 <- margin.table(ShakyBirds, c("Aerial","Aggression"))
ORmat <- function( M ){ ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) }
ORmat(table3)

#marginal over food
table4 <- margin.table(ShakyBirds,c(1,3,4))
table5 <- aperm(table4, c(1,3,2))
fourfold(table5)    

#Investigating Simpson's Paradox
table6 <- aperm(margin.table(ShakyBirds,c(1,3,4)),c(1,3,2))
ORmat(table6[,,1])
ORmat(table6[,,2])
table7 <- margin.table(ShakyBirds, c(1,4))
ORmat(table7)


#LoglInear models
library(MASS)
M_A <- loglm(~Food+Feet*Aerial+Feet*Aggression, data=ShakyBirds)
summary(M_A)
M_A$param

M_B <- loglm(~Feet*Aerial+Feet*Aggression+Feet*Food+
               Food*Aerial+Food*Aggression+
               Aerial*Aggression,data=ShakyBirds)
summary(M_B)
M_A$deviance-M_B$deviance

sat.fit <- loglm(~Aerial*Food*Feet*Aggression, data=ShakyBirds)
n<-sum(ShakyBirds)
step(sat.fit, direction="backward", k=log(n))


#Possum binary data
response<-possum["sex"]
predictors <- possum[c("hdlngth","taill","footlgth")]


possum.glm <- glm(sex~hdlngth+taill+footlgth,
                  family=binomial(link="probit"),data=possum)
predData <- with(possum, data.frame(hdlngth=85,footlgth=63,taill=36))
predData$prediction <- predict(possum.glm, newdata = predData,
                               type = "response")

predict( glm( sex ~ skullw, data = possum, family = binomial ), with( possum, data.frame( skullw = 55 ) ) )
