#Contingency tables
TitanicA <- aperm(margin.table(Titanic, margin=c(1,2,4)),c(2,3,1))

mantelhaen.test(TitanicA, correct=FALSE)

fourfoldplot(TitanicA)

TitanicB <- margin.table(Titanic[3:4,,,], c(1,2,4))

TitanicB_margin <- margin.table(TitanicB, margin=c(1,3))

ORmat <- function( M ){ ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) }

ORmat(TitanicB_margin)      
#OR between class and survived, marginalised over sex, is 0.934

ORmat(TitanicB[,1,])
#OR between class and survival, given male, is 7.85
ORmat(TitanicB[,2,])
#OR between class and survival, given female, is 0.934

#LLMs for dose-result data
library(MASS)

DoseResult <- matrix( c(47, 25, 12,
                        36, 22, 18,
                        41, 60, 55), byrow = TRUE, ncol = 3 )

dimnames( DoseResult ) <- list( Dose=c("High",
                                       "Medium",
                                       "Low"),
                                Result=c("Success",
                                         "Partial",
                                         "Failure") )
DoseResultTable <- addmargins(DoseResult)

(I.fit <- loglm( ~ Dose + Result, data=DoseResult))
#Goodness-of-fit test suggests to reject the independence
#model in favour of the saturated model. Thus conclude
#there is an association between dose level and 
#treatment result

(sat.fit <- loglm(~Dose*Result, data=DoseResult))

(I_param <- I.fit$param)
(sat_param <- sat.fit$param)

exp(I_param$'(Intercept)'+I_param$Dose[2]+I_param$Result[1])
DoseResultTable[2,4]*DoseResultTable[4,1]/DoseResultTable[4,4]

fitted(I.fit)

exp(sat_param$'(Intercept)'+sat_param$Dose[2]+sat_param$Result[1]+sat_param$Dose.Result[2,1])
DoseResult[2,1]

lambdaXY <- sat_param$Dose.Result
lambdaXY[2,1]+lambdaXY[3,2]-lambdaXY[2,2]-lambdaXY[3,1]

local_OR <- function( DR ){
  
  # I and J
  I <- nrow(DR)
  J <- ncol(DR)
  
  # Odds ratio matrix.
  OR_local <- matrix( NA, nrow = I-1, ncol = J-1 )
  for( i in 1:(I-1) ){
    for( j in 1:(J-1) ){
      OR_local[i,j] <- ORmat( M = DR[c(i,i+1), c(j,j+1)] )
    }
  }
  
  return(OR_local)
}  

log(local_OR(DoseResult))


dat <- array(c(79, 68, 5, 17,
               89, 221, 4, 46,
               141, 77, 6, 18,
               45, 26, 29, 21,
               81, 112, 3, 11,
               168, 51, 13, 12), c(2,2,6))
dimnames(dat) <- list(Treatment=c("Success","Failure"),
                      Prognostic_Factor=c("Yes","No"),
                      Clinic=c("A","B","C","D","E","F"))

(hom.assoc <- loglm(~ Treatment*Prognostic_Factor
                    + Prognostic_Factor*Clinic
                    + Treatment*Clinic, data=dat))

(cond.ind.TF <- loglm(~Prognostic_Factor*Clinic
                      + Treatment*Clinic, data=dat))

hom.assoc$deviance

(DG2 <- cond.ind.TF$deviance - hom.assoc$deviance)

(p.value <- 1-pchisq(DG2,1))

sat <- loglm(~Prognostic_Factor*Clinic*Treatment, data=dat)
step(sat, direction='backward', test='Chisq')

(DR.I.fit <- loglm(~ Class + Survived + Sex + Age, data=Titanic))
(DR.sat.fit <- loglm(~Class*Survived*Sex*Age, data=Titanic))  

(Three.fit <- loglm(~Class*Survived*Sex
                    +Class*Survived*Age
                    +Class*Sex*Age
                    +Survived*Sex*Age, data=Titanic))
(Two.fit <- loglm(~Class*Survived + Survived*Sex
                  + Survived*Age + Class*Sex
                  +Sex*Age + Class*Age, data=Titanic))

(DR.DG2 <- Two.fit$deviance - Three.fit$deviance)
(p.value <- 1-pchisq(DG2, 10))

step(DR.sat.fit, direction='backward', test='Chisq')

n <- sum(Titanic)
step(DR.I.fit, scope=~Class*Survived*Sex*Age,
     direction='forward', test='Chisq', k=log(n))
