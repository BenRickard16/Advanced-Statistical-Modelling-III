#Chi-square test for independence on mushroom data
mushroom_data <- matrix(c(101,399,57,487,
                          12,389,150,428),byrow=TRUE,ncol=4)

dimnames(mushroom_data) <- list(Edibility=c('Edible','Poisonous'),
                                Cap_Shape=c('bell','flat','knobbed','convex/conical'))

chisq_Mush <- chisq.test(mushroom_data)
#Strong evidence to reject H_0 of independence between
#edibility and cap shape

#Residual analysis
#Pearson residuals
chisq_Mush$residuals

#Adjusted residuals
chisq_Mush$stdres

#Both suggest bell-capped and knobbed-capped mushrooms
#contribute most to the X^2 test statistic

#GLR test
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

G2mush <- G2(mushroom_data)
G2mush$p.value
G2mush$dev_res
G2mush$Gij2
#p-value close to 0 so strong evidence to reject H_0

#Nominal odds ratio
ORmat <- function( M ){ ( M[1,1] * M[2,2] ) / ( M[1,2] * M[2,1] ) }

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

nominal_OR(mushroom_data)

mosaic(mushroom_data,gp=shading_hcl,residuals_type='Pearson')

#Dose results data
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

#Function for local odds ratio
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

#Function for global odds ratio
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

local_OR(DoseResult)
global_OR(DoseResult)

#Function producing an (I-1)x(J-1) matrix of fourfold
#plots, each corresponding to the submatrices associated
#with each of the (I-1)x(J-1) local ORs
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

ffold_local(DoseResult)

#Linear trend test
#Manually
score_x <- 1:3
score_y <- 1:3

score_xy <- crossprod( t(score_x), t(score_y) )
n <- sum(DoseResult)

sum_xy <- sum( score_xy * DoseResult )
sum_x <- sum( score_x * rowSums(DoseResult) )
sum_y <- sum( score_y * colSums(DoseResult) )
sum_x2 <- sum( score_x^2 * rowSums(DoseResult) )
sum_y2 <- sum( score_y^2 * colSums(DoseResult) )

rxy <- ( n * sum_xy - sum_x * sum_y ) / ( sqrt( n * sum_x2 - sum_x^2 ) * 
                                            sqrt( n * sum_y2 - sum_y^2 ) )

M2 <- (n-1) * rxy^2

p.value <- 1-pchisq(M2,1)

#Writing a function
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

linear.trend(table=DoseResult, x=score_x, y=score_y)

#Titanic data
?Titanic
dim(Titanic)
dimnames(Titanic)
head(Titanic)

#Partial tables cross-classifying Class and Sex for 
#Age=Child and Survived=No
Titanic[,,Age='Child',Survived='No']

#Marginal tables of Class and Sex, and for Survived
margin.table(Titanic, margin=c(1,2))
margin.table(Titanic, margin=4)

#Class could be treated as ordinal --> global ORs could
#be applicable
#Calculating nominal, local, and global marginal ORs for
#Class and Sex having marginalised over Age and Survived
nominal_OR(margin.table(Titanic,margin=c(1,2)))
local_OR(margin.table(Titanic,margin=c(1,2)))
global_OR(margin.table(Titanic,margin=c(1,2)))

#Chi-square test independence of Class and Survival
chisq.test(margin.table(Titanic, margin=c(1,4)))
#Significant evidence of association

#Linear trend test (Class ordinal variable)
linear.trend(table=margin.table(Titanic, margin=c(1,4)),
             x=1:4,y=1:2)

sieve(Titanic)
mosaic(Titanic)
