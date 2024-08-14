#Intro to Contingency Table
DR_data <- matrix( c(41,9,
                     37,13), byrow=TRUE, ncol=2)
dimnames(DR_data) <- list(Dose=c("High", "Low"),
                          Result = c("Success", "Failure"))

DR_contingency_table <- addmargins(DR_data)

DR_prop <- prop.table(DR_data)
DR_prop_table <- addmargins(DR_prop)

DR_prop_1 <- prop.table(DR_data, 1)
prop.table(DR_data, 2)

DR_prop_1_table <- addmargins(DR_prop_1, margin=2)

DR_prop_3_table <- addmargins(DR_prop_1,
                              margin = c(1,2),
                              FUN = list(mean,sum))

#Penguin Data
install.packages("palmerpenguins")
library(palmerpenguins)
head(penguins)

penguins_data <- table(Species = penguins$species, Island = penguins$island)

penguins_table <- addmargins(penguins_data)

penguin_prop <- prop.table(penguins_data)

penguin_prop_1 <- prop.table(penguins_data, 2)
penguin_prop_1_table <- addmargins(penguin_prop_1, 1)
                             
penguins_2_table <- addmargins(penguins_data,2)
penguins_prop_2_table <-prop.table(penguins_2_table,2)

penguins_prop_2_table <- addmargins(penguins_prop_2_table, 1)

#Chi-Square Test for Independence
chisq.test(DR_data, correct=FALSE)
chisq.test(DR_data)

chisq.test(DR_data)$expected

#Testing for Independence in Penguin Data
chisq.test(penguins_data, correct=FALSE)
#p-value is extremely small ==> evidence of association

#Data Visualisation
barplot(DR_prop)

barplot(DR_prop, density=70)
barplot(DR_prop, density=30)
barplot(DR_prop, density=0)

barplot(DR_prop, legend.text = T, xlab='Treatment Outcome', ylab='Proportions',
        main='Comparison of Dose by Response', density=30,
        args.legend=list(x=3, y=3))


fourfoldplot(DR_data)

install.packages("vcd")
library(vcd)
sieve(DR_data)
sieve(DR_data, shade=T)
sieve(DR_data, sievetype='expected', shade=T)

#Odds Ratios
odds_ratio <- function(pA, pB){
  a <- pA/(1-pA)
  b <- pB/(1-pB)
  a/b
}

odds_ratio(1/3,1/2)

OR_cont_table <- function(M){
  (M[1,1]*M[2,2])/(M[1,2]*M[2,1])
}

OR_cont_table(DR_data)

AB <- matrix(c(4,3,0,0), ncol=2)
OR_cont_table(AB)

OR_cont_table_1 <- function(M){
  
  if ((prod(rowSums(M))==0 | prod(colSums(M))==0)){
    stop('At least one row sum or column sum of M is equal
         to zero, hence the odds ratio is undefined')
  }
  else{
    OR_cont_table(M)
  }
}

OR_cont_table_1(DR_data)
OR_cont_table_1(AB)

OR_cont_table_2 <- function(M){
  if ((prod(rowSums(M))==0 | prod(colSums(M))==0)){
    M <- M+0.5*matrix(1, nrow=2, ncol=2)
    warning('At least one row sum or column sum of the supplied
            matrix was equal to zero, hence an amendment of 0.5
            was added to the value of each cell prior to 
            calculating the odds ratio')
    return(OR_cont_table(M))
  }
  else{
    OR_cont_table(M)
  }
}

OR_cont_table_2(DR_data)
OR_cont_table_2(AB)

#Analysis of mushroom data
mushroom_data <- matrix(c(101,399,57,487,
                          12,389,150,428),byrow=TRUE,ncol=4)

dimnames(mushroom_data) <- list(Edibility=c('Edible','Poisonous'),
                                Cap_Shape=c('bell','flat','knobbed','convex/conical'))
mushroom_data

#Looking at proportion of each shape of mushroom that
#are edible or poisonous
mushroom_table <- addmargins(mushroom_data,2,FUN=mean)
mushroom_table <- prop.table(mushroom_table,2)
mushroom_table <- addmargins(mushroom_table,1)
mushroom_table

#Chi-square test
chisq.test(mushroom_data)

#Barplot
barplot(prop.table(mushroom_data,margin=2),density=50,
        main='Comparison of Edibility by Cap Shape',cex.main=0.8,
        xlab='treatment outcome',ylab='proportions')

#Sieve diagram
sieve(mushroom_data)
#Can see there are more edible bell mushrooms in our sample
#than would be expected under assumption of independence
