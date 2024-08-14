#Constructing the 2x2 contingency table
DR_data <- matrix( c(41,9,
                     37,13), byrow=TRUE, ncol=2)
dimnames(DR_data) <- list(Dose=c("High","Low"),
                          Result=c("Success","Failure"))

#Adding row and column sums
DR_contingency_table <- addmargins(DR_data)

#Creating a table of proportions
DR_prop <- prop.table(DR_data)
DR_prop_table <- addmargins(DR_prop)

#Row conditional proportions
DR_prop_1 <- prop.table(DR_data,1)
DR_prop_1_table <- addmargins(DR_prop_1, 2)

#Mean over the rows and sum over the columns
DR_prop_3_table <- addmargins(DR_prop_1,
                              margin=c(1,2),
                              FUN=list(mean,sum))

library(palmerpenguins)
#Tabulate the dataframe
penguins_data <- table(Species=penguins$species,
                       Island=penguins$island)

#Adding row and column sums
penguins_table <- addmargins(penguins_data)

#Table of proportions
penguins_prop <- prop.table(penguins_data)

#Column-conditional probabilities
penguins_prop_1 <- addmargins(prop.table(penguins_data,2),1)

#Adding overall proportions of penguin specie to
#appear in a final column
penguins_prop_2_table <- addmargins(penguins_data,2)
penguins_prop_2_table <- addmargins(prop.table(penguins_prop_2_table,2),1)

#Chi-square test of independence
chisq.test(DR_data, correct=FALSE)
chisq.test(penguins_data, correct=FALSE)
#Extremely small p-value

#Barplots
barplot(DR_prop,density=90,main="Comparison of Dose by Response",
        xlab="Treatment Outcome",ylab="Proportions", legend.t=T)

#Sieve Diagrams
library(vcd)
sieve(DR_data)
sieve( DR_data, shade = T )

#Fourfold plot
fourfoldplot( DR_data )

#Odds Ratio of success of event A against success of event B
OR <- function(pA, pB){(pA/(1-pA))/(pB/(1-pB))}

#Odds ratio of 2x2 contingency table
ORmat <- function( M ){ ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) }

#Function that can fix errors when have 0s top and bottom
OR_cont_tab_2 <- function( M ){ 
  
  M_row_sum <- rowSums( M )
  M_col_sum <- colSums( M )
  
  if( prod( M_row_sum ) == 0 | prod( M_col_sum ) == 0 ){
    M <- M + 0.5 * matrix( 1, nrow = 2, ncol = 2 )
    OR <- ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] )
    warning( "At least one row sum or column sum of the supplied 
             matrix M was equal to zero, hence an amendment of 0.5 was added 
             to the value of each sum prior to calculating the odds ratio." )
    return( OR )
  }
  else{ 
    ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) 
  }
}

#Mushroom data
# We create a matrix with the data in.
mushroom_data <- matrix( c(101, 399, 57, 487, 
                           12, 389, 150, 428 ), byrow = TRUE, ncol = 4 )

# Add dimension names as follows.
dimnames( mushroom_data ) <- list( Edibility = c("Edible", "Poisonous"),
                                   Cap_Shape = c("bell", "flat", "knobbed",
                                                 "convex/conical") )

#Let's look at the proportion of each shape of mushroom that are 
#edible or poisonous.
mushroom_table <- addmargins( mushroom_data, 2, FUN = mean )
mushroom_table <- prop.table( mushroom_table, 2 )
mushroom_table <- addmargins(mushroom_table, 1 )
mushroom_table

#Chi-square test.
chisq_Mush <- chisq.test( mushroom_data )

#Can obtain Pearson and adjusted residuals
chisq_Mush$residuals
chisq_Mush$stdres

#GLR test function
G2 <- function( data ){
  # computes the G2 test of independence 
  # for a two-way contingency table of
  # data: IxJ matrix
  X2 <- chisq.test( data )
  Ehat <- X2$expected
  df <- X2$parameter
  
  term.G2 <- data * log( data / Ehat ) 
  term.G2[data==0] <- 0 # Because if data == 0, we get NaN
  
  Gij2 <- 2 * term.G2 # Individual cell contributions to G2 statistic.
  dev_res <- sign( data - Ehat ) * sqrt( abs( Gij2 ) )
  G2 <- sum( Gij2 ) # G2 statistic
  p <- 1 - pchisq( G2, df ) 
  return( list( G2 = G2, df = df, p.value = p, 
                Gij2 = Gij2, dev_res = dev_res ) ) 
}

#Function for nominal odds ratio (reference cell IJ)
nominal_OR <- function( data, ref_x = nrow( data ), ref_y =  ncol( data ) ){
  
  # I and J
  I <- nrow(data)
  J <- ncol(data)
  
  # Odds ratio matrix.
  OR_reference_IJ <- matrix( NA, nrow = I, ncol = J )
  for( i in 1:I ){
    for( j in 1:J ){
      OR_reference_IJ[i,j] <- ORmat( M = data[c(i,ref_x), c(j,ref_y)] )
    }
  }
  
  OR_reference <- OR_reference_IJ[-ref_x, -ref_y, drop = FALSE]
  
  return(OR_reference)
  
}

#Add residual information to mosaic plot
mosaic( mushroom_data, 
        gp = shading_hcl, 
        residuals_type = "Pearson" )

#Functions for local and global ORs
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

global_OR <- function( DR ){
  
  # I and J
  I <- nrow(DR)
  J <- ncol(DR)
  
  # Odds ratio matrix.
  OR_global <- matrix( NA, nrow = I-1, ncol = J-1 )
  for( i in 1:(I-1) ){
    for( j in 1:(J-1) ){
      OR_global[i,j] <- ORmat( M = matrix( c( sum( DR[1:i,1:j] ),
                                              sum( DR[(i+1):I,1:j] ),
                                              sum( DR[1:i,(j+1):J] ),
                                              sum( DR[(i+1):I,(j+1):J] ) ), 
                                           nrow = 2 ) )
    }
  }
  
  return(OR_global)
  
}  

#Function to make four fold plots for local ORs
ffold_local <- function ( data ){
  
  # I and J
  I <- nrow(data)
  J <- ncol(data)
  
  par( mfrow = c(I-1, J-1) )
  for( i in 1:(I-1) ){
    for( j in 1:(J-1) ){
      sub_data <- data[c(i,i+1),c(j,j+1)]
      fourfoldplot( sub_data )
    }
  }
  
}

#Dose Result data
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

#Linear trend test
linear.trend <- function( table, x, y ){
  # linear trend test for a 2-way table
  # PARAMETERS:
  # freq: vector of the frequencies, given by rows
  # NI: number of rows
  # NJ: number of columns
  # x: vector of row scores
  # y: vector of column scores
  # RETURNS:
  # r: Pearson’s sample correlation
  # M2: test statistic
  # p.value: two-sided p-value of the asymptotic M2-test
  NI <- nrow( table )
  NJ <- ncol( table )
  
  rowmarg <- addmargins( table )[,NJ+1][1:NI]
  colmarg <- addmargins( table )[NI+1,][1:NJ]
  n <- addmargins( table )[NI+1,NJ+1]
  
  xmean <- sum( rowmarg * x ) / n
  ymean <- sum( colmarg * y ) / n 
  xsq <- sqrt( sum( rowmarg * ( x - xmean )^2 ) )
  ysq <- sqrt( sum( colmarg * ( y - ymean )^2 ) ) 
  
  r <- sum( ( x - xmean ) %*% table %*% ( y - ymean ) ) / ( xsq * ysq )
  M2 = (n-1)*r^2
  p.value <- 1 - pchisq( M2, 1 ) 
  return( list( r = r, M2 = M2, p.value = p.value ) ) 
}


score_x <- 1:3
score_y <- 1:3
linear.trend( table = DoseResult, x = score_x, y = score_y )


#Titanic data
Titanic
dim( Titanic )
dimnames( Titanic )

#Partial table cross-classifying Class and Sex for 
#Age=Child and Survived=No
Titanic[,, Age="Child", Survived="No"]

#Marginal table of Class and Sex, summing over Age and
#Survived
margin.table( Titanic, margin = c(1,2) )

#Nominal, local and global marginal odds ratios for Class
#and Sex having marginalised over Age and Survived
nominal_OR( margin.table( Titanic, margin = c(1,2) ) )
local_OR( margin.table( Titanic, margin = c(1,2) ) )
global_OR( margin.table( Titanic, margin = c(1,2) ) )

#Marginalise over age and permute to give 2x2x4
margin_age <- margin.table( Titanic, margin = c(1,2,4) )
TitanicA <- aperm(margin_age, c(2,3,1))

#Mantel-Haenszel Test
mantelhaen.test( TitanicA, correct = FALSE )

#Four fold plot
par(mfrow=c(1,1))
fourfoldplot(TitanicA)

#Log Linear Models
library(MASS)

#Fit independence LLM to Dose-Result data, with zero-sum
#constraints
I.fit <- loglm( ~ Dose + Result, data = DoseResult )
#Likelihood ratio gives GLR test statistic against fully
#saturated model

#Fit the saturated model
sat.fit <- loglm(~Dose*Result, data=DoseResult)

#To access the MLE of parameters
I_param <- I.fit$param
sat_param <- sat.fit$param

#To get the expected values under independence model
fitted(I.fit)

#Recall Local ORs can be directly calculated from the
#interaction parameters
#e.g. for cell (2,1)
lambdaXY <- sat_param$Dose.Result
lambdaXY[2,1] + lambdaXY[3,2] - lambdaXY[2,2] - lambdaXY[3,1]
#Verifying
log( local_OR( DoseResult ) )

#Extended Dose-Result data
dat <- array(c(79, 68, 5, 17,
               89, 221, 4, 46,
               141, 77, 6, 18,
               45, 26, 29, 21,
               81, 112, 3, 11,
               168, 51, 13, 12), c(2,2,6))
dimnames(dat) <- list(Treatment=c("Success","Failure"),
                      Prognostic_Factor=c("Yes","No"),
                      Clinic=c("A","B","C","D","E","F"))

#Fit the homogeneous associations and conditional
#independence (of Prognostic_Factor and Treatment on Clinic)
#models as follows
( hom.assoc <- loglm(~ Treatment*Prognostic_Factor 
                     + Prognostic_Factor * Clinic 
                     + Treatment * Clinic, 
                     data=dat )  )

( cond.ind.TF <- loglm(~ Prognostic_Factor * Clinic 
                       + Treatment * Clinic, data=dat ) )

#Likelihood Ratio shows the GLR goodness-of-fit test 
#against the fully saturated model of dimension, q, where
#q is the number of variables
hom.assoc$deviance

#To get the GLR test statistic between these two models
( DG2 <- cond.ind.TF$deviance - hom.assoc$deviance )
#Subtract the test statistic for M1 from M0, where M0 is
#nested in M1

#To get the p-value
( p.value <- 1 - pchisq(DG2, 1) )

#Backwise selection with AIC
sat <- loglm(~ Prognostic_Factor * Clinic * Treatment, data=dat )
step( sat, direction="backward", test = "Chisq" )
step( sat, direction="backward")

#Titanic data
#Fitting independent and saturated models
DR.I.fit <- loglm( ~ Class + Survived + Sex + Age, data=Titanic)
DR.sat.fit <- loglm( ~ Class*Survived*Sex*Age, data=Titanic)

#Testing M0 all 2-way interactions model against M1 all
#3-way interaction models
( Three.fit <- loglm( ~ Class*Survived*Sex 
                      + Class*Survived*Age 
                      + Class*Sex*Age 
                      + Survived*Sex*Age, data = Titanic ) ) 
( Two.fit <- loglm( ~ Class*Survived + Survived*Sex 
                    + Survived*Age + Class*Sex 
                    + Sex*Age + Class*Age, data = Titanic ) ) 
#Test statistic
( DR.DG2 <- Two.fit$deviance - Three.fit$deviance )
#p-value
# The p-value for testing H_0: M_0 against alternative H_1: M_1 is then given by
( p.value <- 1 - pchisq(DR.DG2, 10) )

#Backwards selection with AIC
step( DR.sat.fit, direction="backward", test = "Chisq" )

#Forwards selection with BIC
n <- sum( Titanic )
step( DR.I.fit, scope = ~ Class*Survived*Sex*Age, direction="forward", 
      test = "Chisq", k = log(n) )


#Binary regression models
shuttle <- read.table("shuttle.asc", header=TRUE)
getwd()

#Linear model
fit1 <- lm(td~temp, data=shuttle)
plot(shuttle$temp,shuttle$td, ylab='Probability of Thermal Overstrain', xlab='Temperature')
abline(lm(td~temp, data=shuttle), col='red')

#Logistic regression model with logit link
shuttle.glm <- glm(td~temp, family=binomial(link=logit),data=shuttle)

#To extract the parameters
( coefficients <- as.numeric(shuttle.glm$coef) )

#Function returning probability for any temperature
shuttle.fit2 <- function(temp, model){
  coefficients <- as.numeric(model$coef)
  exp(coefficients[1] + coefficients[2] * temp) /
    ( 1 + exp(coefficients[1] + coefficients[2] * temp) )
}

#Adding the fit to the plot
lines(30:90, shuttle.fit2(30:90, model = shuttle.glm), lty = 2)
legend(70, 0.8, c("Linear model", "Logistic model"), lty=c(1,2))

#What temperature gives a probability of thermal strain 0.5
shuttle.fit.inverse <- function(prob, model){
  coefficients <- as.numeric(model$coef)
  (log( prob / ( 1- prob ) ) - coefficients[1]) / coefficients[2]
}

shuttle.fit.inverse(0.5, model = shuttle.glm )

#Predict the probability of thermal overstrain with a
#temperature of 31
shuttle.fit2(31, model = shuttle.glm)

#Use a probit link function
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

#Must tell R that rank is categorical data
graduate$rank <- factor(graduate$rank)

#Fit a logistic regression
graduate.glm <- glm(admit ~ gre + gpa + rank, data = graduate,
family = "binomial")
#The logistic regression coefficients in the R output give
#the change in the log odds of the outcome for a one unit
#increase in the predictor variable

#To get the odds ratio
exp(coef(graduate.glm))

#To make a prediction probability
predData <- with(graduate, data.frame(gre = 700, gpa = 3.7, rank = factor(2)))

predData$prediction <- predict(graduate.glm, newdata = predData,
type = "response")
predData

#With a probit function instead
graduate.glm.probit <- glm(admit ~ gre + gpa + rank, data = graduate, 
                           family = "binomial"(link="probit"))
summary(graduate.glm.probit)

predData$prediction2 <- predict(graduate.glm.probit, newdata = predData, 
                                type = "response")
