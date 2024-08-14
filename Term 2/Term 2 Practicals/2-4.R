#Popularity data
install.packages('haven')
library(haven)
pop.rawdata <- read_sav(file="https://github.com/MultiLevelAnalysis/Datasets-third-edition-Multilevel-book/blob/master/chapter%202/popularity/SPSS/popular2.sav?raw=true")

require(gee)
require(ggplot2)

#Creating a dataframe from first 6 columns
pop.data <- data.frame(pop.rawdata[,1:6])
head(pop.data)

#Changing the column names
colnames(pop.data) <- c('pupil','class', 'extraversion', 'gender', 'experience', 'popularity')

#Exploratory analysis
ggplot(data=pop.data, aes(popularity)) +
  geom_histogram(bins=15)
#Hence we can model the response as a Gaussian

#Viewing distribution of the predictor variables
ggplot(data=pop.data, aes(x=extraversion)) +
  geom_bar(stat="count")
ggplot(data=pop.data, aes(x=gender)) +
  geom_bar(stat="count")
ggplot(data=pop.data, aes(x=experience)) +
  geom_bar(stat="count")
#Shape of distribution of predictor variables not important from 
#statistical modelling point of view

#Plot pop. vs extraversion with simple linear regression line
ggplot(data=pop.data, aes(x=extraversion, y=popularity)) +
  geom_point(size=1, alpha=.8) +
  geom_smooth(method='lm', se=FALSE, col='red', linewidth=1, alpha=0.8)

#Class-specific linear regression lines
ggplot(data = pop.data,
       aes(x = extraversion, y = popularity, colour = as.factor(class))) +
  geom_jitter(size=0.8) + # to add some random noise for plotting purposes
  labs(title = "Popularity versus extraversion", subtitle = "by pupil") +
  theme(legend.position = "none") +
  geom_smooth(method='lm', se=FALSE)

#Modelling
#Simple linear regression
pop.simple <- lm(popularity~extraversion, data=pop.data)
summary(pop.simple)

#Fit marginal models for popularity vs extraversion with independent,
#exchangeable, and unstructured working correlation matrix
pop.gee0 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='independence')
pop.gee0
summary(pop.gee0)$coef
#Equivalent to the simple linear regression

pop.gee1 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='exchangeable')
pop.gee1 
summary(pop.gee1)$coef
#Parameter estimate changed a lot
#SE for intercept slightly increased, and for extraversion slightly decreased

pop.gee2 <- gee(popularity~extraversion, data=pop.data, id=class, 
                corstr='unstructured')
pop.gee2
summary(pop.gee2)$coef
#Unstructured and exchangeable models give similar parameter estimates
#SEs of latter model a bit smaller, and differences in correlation matrix are
#large

#Plot regression lines from exchangeable and unstructure
pred1 <- predict(pop.gee1)
frame1 <- unique(data.frame(extraversion=pop.data$extraversion, pred1=pred1))

pred2 <- predict(pop.gee2)
frame2 <- unique(data.frame(extraversion=pop.data$extraversion, pred2=pred2))


ggplot(data = pop.data, aes(x = extraversion, y = popularity)) +
  geom_point(size = 1,alpha = .8) +
  geom_smooth(method = "lm", # to add regression line
              col = "red", size = 1, alpha = .8, se=FALSE) +
  geom_line(data=frame1, aes(x=extraversion, y=pred1), col="Blue") +
  geom_line(data=frame2, aes(x=extraversion, y=pred2), col="Green")

#Now with all covariates
pop.gee0a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="independence", data=pop.data)
pop.gee0a
summary(pop.gee0a)$coef


pop.gee1a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="exchangeable", data=pop.data)
pop.gee1a
summary(pop.gee1a)$coef


pop.gee2a <- gee(popularity~extraversion+gender+experience, id=pop.data$class,
                 corstr="unstructured", data=pop.data)
pop.gee2a
summary(pop.gee2a)$coef