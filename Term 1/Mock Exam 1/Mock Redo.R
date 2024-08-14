library(MASS)

car_data <- table(type=Cars93$Type,
                  airbags=Cars93$AirBags,
                  MTA=Cars93$Man.trans.avail,
                  origin=Cars93$Origin)
dim(car_data)


car_dataA <- margin.table(car_data, margin=c(1,4))
#1
carSums <- addmargins(car_dataA)
#2
carMarg <- prop.table(addmargins(car_dataA,1),1)
#3
chisq.test(car_dataA, correct=FALSE)
#Error message as some observations 0 or very small

#4
car_dataB <- matrix(c(7,9,16,4,10,12,7,14,8,6),
                    byrow=TRUE, ncol=2)
dimnames(car_dataB) <- list(type=c('Compact', 'Large/Van', 'Midsize', 'Small', 'Sporty'),
                            origin=c('USA', 'non-USA'))
CST <- chisq.test(car_dataB, correct=FALSE)
#10.165

#5
#Not enough evidence

#6
mosaic(car_dataB, gp = shading_hcl, residuals = CST$stdres )

#7
car_dataC <- margin.table(car_data, c(1,3,4))
car_dataC <- aperm(car_dataC, c(2,3,1))
mantelhaen.test(car_dataC, correct=FALSE)
#p-value=0.00161

#8
#Significant evidence against MTA and origin conditionally
#independent of type, having marginalise over airbags

#9
car_dataC
#(9*8)/(1*4)=18

#11
fourfoldplot(car_dataC[,,3])

#12
margin.table(car_dataC,c(1,3))
#(13*14)/(2*9) = 10.111

#13
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

nominal_OR(margin.table(car_dataC, margin=c(1,3)),1,3)
#3


car_dataD <- margin.table(car_data, margin=c("airbags",
                                             "MTA",
                                             "origin"))
#16
M_A <- loglm(~origin*airbags+MTA, data=car_dataD)

#17
M_A$param$MTA["No"]
#-0.323

#18
#Difference between the mean log expected cell count for No over other categories and overall mean log expected cell count

#19
#2

#20
M_B <- loglm(~MTA*airbags+origin*airbags,
             data=car_dataD)
DR.DG2 <- M_A$deviance - M_B$deviance
#3.683

#21
1-pchisq(3.683,2) #= p-value
#0.159

#23
sat <- loglm(~MTA*airbags*origin, data=car_dataD)
step( sat, direction="backward")
#Joint independence of MTA and origin from airbags

#25
step( sat, direction="backward", k=log(sum(car_dataD)))

#26
#Same model