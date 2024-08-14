### Mock Assessment Solutions ###

# Answers to multiple choice/numerical questions provided on Ultra.

## Data ##

# Library MASS
library(MASS)
# Cars93 data
head(Cars93)
# We tabulate several variables from the car dataset as follows.
( car_data <- table( type = Cars93$Type, 
                     airbags = Cars93$AirBags, 
                     MTA = Cars93$Man.trans.avail, 
                     origin = Cars93$Origin ) )
( dim(car_data) )
( dimnames(car_data) )
# Look in the help file to find out information about the dataset and in particular these variables.
?Cars93

## Two-Way Contingency Tables - 8 marks ##

( car_dataA <- margin.table( car_data, margin =  c("type", "origin") ) )

# Question 1 #

( carSums <- addmargins( car_dataA ) )

# Question 2 #

( carMarg <- addmargins( car_dataA, 1 ) )
( carMarg <- prop.table( carMarg, 1 ) )

# Question 3 #

# Acceptable answers along the lines of
# - The expected values for some of the rows and columns are small.
# - There is a zero in the data / small observed values.

# Question 4 #

( LargeVan <- car_dataA["Large",] + car_dataA["Van",] )
( car_dataB <- rbind( car_dataA[c("Compact", "Midsize", "Small", "Sporty"),], "LargeVan" = LargeVan ) )
dimnames( car_dataB ) <- list( type = c("Compact", "Midsize", "Small", "Sporty", "LargeVan"), origin = c("USA", "non-USA") )
( CST <- chisq.test( car_dataB, correct = FALSE ) )
# 10.165

# Question 5 #

# No

# Mosaic Plot - Question 6 #

( LargeVan <- car_dataA["Large",] + car_dataA["Van",] )
( car_dataB <- rbind( car_dataA[c("Compact", "Midsize", "Small", "Sporty"),], "LargeVan" = LargeVan ) )
# If you want to re-add the dimension names to the new object car_dataB, the easiest way is to do it manually...(not required for marks)
dimnames( car_dataB ) <- list( type = c("Compact", "Midsize", "Small", "Sporty", "LargeVan"), origin = c("USA", "non-USA") )
( CST <- chisq.test( car_dataB, correct = FALSE ) )
library(vcd)
mosaic( car_dataB,
      #  shade = TRUE)
     gp = shading_hcl,
    residuals = CST$stdres )

# Question 7 #
( car_dataC <- margin.table( car_data, margin =  c("type", "MTA", "origin") ) )
( car_data_perm <- aperm( car_dataC, perm = c(2,3,1) ) )
# 0.00161 without continuity correction.
# If continuity correction was used, then you would obtain 0.00464.

# Question 8 #

# The test provides evidence that MTA and origin are not conditionally independent of type.

# Question 9 #

ORmat <- function( M ){ ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) }
ORmat( M = car_data_perm[,,"Midsize"] )
# 18

# Question 10 #

# Conditional on type = Midsize, the sample odds of a car that does not have a manual transmission version available being of USA origin is x times that of a car that does have a manual transmission version available.
# Conditional on type = Midsize, the sample odds of a car of non-USA origin having a manual transmission version available is x times that of a car of USA origin.

# Fourfold Plot - Question 11 #

( car_dataC <- margin.table( car_data, margin =  c("type", "MTA", "origin") ) )
( car_data_perm <- aperm( car_dataC, perm = c(2,3,1) ) )
fourfoldplot( car_data_perm[,,"Midsize"] )

# Question 12 #

( car_data_marg <- margin.table( car_data, margin =  c("type", "MTA") ) )

# We need the nominal OR function.
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
  
  OR_reference <- OR_reference_IJ[-ref_x, -ref_y]
  
  return(OR_reference)
  
}

# Nominal odds ratios given by (note the reference categories, must be defined by numerical position).
( nominalORs <- nominal_OR( data = car_data_marg, ref_x = 3, ref_y = 1 ) )

# 10.111

# Question 13 #

# 3

# Question 14 #

# There exist observed zeros in the contingency table.

# Question 15 #

# Use a continuity correction that involves adding a small number such as 0.5 to each cell of the corresponding 2x2 table that would otherwise result in a 0 or infinite value.
# Combine some of the categories of type in the contingency table together.

# Question 16 #
( car_dataD <- margin.table( car_data, margin =  c("airbags", "MTA", "origin") ) )
( M_A <- loglm( ~ airbags*origin + MTA, data = car_dataD ) )

# Question 17 #

M_A$param$MTA
# -0.323

# Question 18 # 

# -0.323 is the difference between the mean log expected cell count for MTA = no over all values of origin and airbags, and the overall mean log expected cell count.  

# Question 19 #

# 2

# Question 20 #

# 3.683

# Question 21 #

# 0.159

# Question 22 #

# p-value is 0.159 to 3sf, suggesting that the joint independence model (Model A) is more appropriate.

# Question 23 #

# joint independence of origin and MTA on airbags.

# Question 24 #

# 4.625

# Question 25 #

( car_sat <- loglm( ~ airbags*origin*MTA, data = car_dataD ) )
n <- sum( car_data )
step( car_sat, direction="backward", k = log(n) )

# Question 26 #

# 0

