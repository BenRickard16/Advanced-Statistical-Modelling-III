#install.packages("gamlss.data")
library( "gamlss.data" )

data( "polio" )
uspolio <- as.data.frame( matrix( c( 1:168, t( polio ) ), ncol = 2 ) )
colnames( uspolio ) <- c("time", "cases")

head(uspolio)

#Poisson model with linear time trend
polio.glm <- glm( cases ~ time, family = poisson( link = log ), data = uspolio )

#Look at the summary
summary( polio.glm )

#Plotting the model
plot(1970 + ((uspolio$time - 1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time - 1)/12), polio.glm$fitted)

#Poisson model with linear trend and seasonal (annual) component
polio1.glm<- glm(cases~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12)),
                 family=poisson(link=log), data=uspolio)

summary(polio1.glm)

plot(1970 + ((uspolio$time - 1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time - 1)/12), polio1.glm$fitted,col=2)

#Poisson model with linear trend and seasonal (annual + sixmonthly)
#component
polio2.glm<- glm(cases~time + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                 + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
                 family=poisson(link=log), data=uspolio)

summary(polio2.glm)

plot(1970 + ((uspolio$time - 1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time - 1)/12), polio2.glm$fitted,col=3)

#Want to add temperature data to the plot
#Average annual temperature data over the 14 years.
temp_data <- rep(c(5.195, 5.138, 5.316, 5.242, 5.094, 5.108, 5.260, 5.153, 
                   5.155, 5.231, 5.234, 5.142, 5.173, 5.167), each = 12 )
#Scale the data so that it plots nicely.
scaled_temp = 10 * (temp_data - min(temp_data))/(max(temp_data) - min(temp_data))
uspolio$temp = scaled_temp
#Plot temperature data against cases data to see interest.
plot(1970 + ((uspolio$time - 1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time - 1)/12), uspolio$temp, col="red")

#Poisson GLM with temp data
# Construct GLM.
polio3.glm<- glm(cases~time + temp + I(cos(2*pi*time/12)) + I(sin(2*pi*time/12))
                 + I(cos(2*pi*time/6)) + I(sin(2*pi*time/6)), 
                 family=poisson(link=log), data=uspolio)

summary(polio3.glm)

plot(1970 + ((uspolio$time - 1)/12), uspolio$cases, type="h")
lines(1970 + ((uspolio$time - 1)/12), polio3.glm$fitted, col="red")

#Compare to simple moving average
#Size of averaging window.
#Try m = 3, 6, 12, 60, 120
m = 60

MA = rep(0, length(uspolio$time))

for (time in uspolio$time)
{
  times = time:min(time + m - 1, length(uspolio$time))
  n = length(times)
  sum = 0
  
  for (newtime in times)
  {
    sum = sum + uspolio$cases[newtime]
  }
  
  MA[time] = sum / m
}


plot(1970 + ((uspolio$time - 1)/12), MA, type = "l")
lines(1970 + ((uspolio$time - 1)/12), 0.2*uspolio$temp, col="red")

