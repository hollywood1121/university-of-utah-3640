#4.8

MCD<-read.csv(file.choose()) #EX04-12MCD
library(lubridate)
MCD$Days<-mdy(MCD$Date)
plot(MCD$Days,MCD$Price,xlab = "Date", ylab = "Price")
abline(lm(MCD$Price~MCD$Days),col="red")


#time series analysis, it needs to tell you about something of the past. its how you forecast. tends to be not independent.
for (i in 2:1126) {
  MCD[i,4]<-MCD[i-1,2]
}

#plot the prices of yesterday. this is called lagging your variables, so you can use this trend to make decisions.
plot(MCD[2:1126,2],MCD[2:1126,4],xlab="Price",ylab="Lag Price")

#the point of 4.8 to determine was to show the price graphs. these price graphs help you see trends associated with price.

#4.9
#see if price differences are independent
MCD<-read.csv(file.choose()) #EX04-12MCD
MCD$Days<-mdy(MCD$Date)
#minus the prices from the current dya minus the previous day
for (i in 2:1126) {
  MCD[i,4]<-(MCD[i,2]-MCD[i-1,2])
}

#plot the price changes
plot(MCD[2:1126,3],MCD[2:1126,4],xlab="Date",ylab="Price Change")
#notice how the graph doesn't really give us any data, they are independent 
# in nature due to the spread not being able to determine anything. 
# this makes the dependent  data now independent.

# correlations matter most, the histogram is simply meant to show you can draw
# more analysis from it rather than just getting stuck in one format of graph

# if you run this info through a histogram you can see the variance from 0.
# its not the main point but it help drives the point that independent variables can be seein in a different light.
hist(MCD[2:1126,4])


for (i in 2:1126) {
  MCD[i,5]<-(MCD[i-1,4])
}
plot(MCD[2:1126,4],MCD[2:1126,5],xlab="Price Change",ylab="Previous Price Change")

# the point of 4.9 was to illustrate with the price minuses, that there isn't any correlation between the change of price and ...?


#4.12
MCD<-read.csv(file.choose()) #EX04-12MCD
MCD$Days<-mdy(MCD$Date)
for (i in 2:1126) {
  MCD[i,4]<-(MCD[i,2]-MCD[i-1,2])
}

# MCD$V4[MCD$V4>0] shows all values in V4, if the value is greater than 0. 
# we look at the length of these all observations because there are 
length(MCD$V4[MCD$V4>0])/length(MCD$V4) #proportion of positive price changes

#number of price changes divided by total change
length(MCD$V4[MCD$V4<0])/length(MCD$V4) #proportion of negative price changes
length(MCD$V4[MCD$V4==0])/length(MCD$V4) #proportion of no changes

# why are these good approximations?

# because our data set is large, we know that based on all the data we have,
# we have a reasonable assumption that the change will be positive will be higher based upon our data set.
# the random event of these independent variables continually happening is happening somewhat often, 
# meaning these approximations are a good data set.
