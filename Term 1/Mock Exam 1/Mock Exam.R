library(MASS)
head(Cars93)
?Cars93

car_data <- table(type=Cars93$Type,
                  airbags=Cars93$AirBags,
                  MTA=Cars93$Man.trans.avail,
                  origin=Cars93$Origin)

dim(car_data)
dimnames(car_data)

#Marginal table for type and origin
car_dataA <- margin.table(car_data, margin=c('type','origin'))
carSums <- addmargins(car_dataA)                          
 
carMarg <- prop.table(car_dataA, margin=1)
carMarg <- prop.table(addmargins(carMarg, margin=1), 1)

chisq.test(car_dataA, correct=FALSE)

car_dataB <- matrix(c(7,9,16,4,10,12,7,14,8,6),
                    byrow=TRUE, ncol=2)
dimnames(car_dataB) <- list(type=c('Compact', 'Large/Van', 'Midsize', 'Small', 'Sporty'),
                            origin=c('USA', 'non-USA'))

CST <- chisq.test(car_dataB, correct=FALSE)

library(vcd)

mosaic(car_dataB)

mosaic(car_dataB, shade=TRUE, gp=shading_hcl, residuals=CST$stdres)
                                                       
car_dataC <- aperm(margin.table(car_data, margin=c(1,3,4)),c(2,3,1))

mantelhaen.test( car_dataC, correct = FALSE )                          

fourfoldplot(car_dataC[,,3])                 

margin.table(car_data, margin=c(1,3))

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

nominal_OR(margin.table(car_data, margin=c(1,3)),3,1)            

car_dataD <- margin.table(car_data, margin=c("airbags","MTA","origin"))

#[MTA, airbags*origin]
M_A <- loglm(~origin  + airbags*MTA, data=car_dataD)

M_A$param

#[MTA*airbags, origin*airbags]
M_B <- loglm(~MTA+airbags+origin+MTA*airbags+origin*airbags,
             data=car_dataD)
DR.DG2 <- M_A$deviance - M_B$deviance
p.value <- 1-pchisq(DG2, 8)

M_sat <-loglm(~MTA*origin*airbags, data=car_dataD)
step(M_sat, direction='backward', test='Chisq')
step(M_sat, direction='backward', test='Chisq',k=log(n))

loglm(formula = ~MTA + origin + airbags + MTA:origin, data = car_dataD)$deviance
