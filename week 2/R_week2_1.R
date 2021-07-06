#2.4
setwd("c:\\Users\\nicka\\Desktop\\econ3640")


edspend<-read.csv(file.choose()) #EG02-04EDSPEND
plot(edspend$Population,edspend$Spending,col="red",xlab="Population",ylab="Spending")
linearmodel<-lm(edspend$Spending ~ edspend$Population) #linear model: intercept and slope parameters
coef<-linearmodel$coefficients #store the coefficients
abline(coef[1],coef[2]) #plot linear model

plot(edspend$Population,edspend$Spending,col="red",xlab="Population",ylab="Spending")
lines(smooth.spline(edspend$Population,edspend$Spending,spar = 2/3)) #too good, not informative

scatter.smooth(edspend$Population,edspend$Spending,col="red",xlab="Population",ylab="Spending") #fits well, good summary

plot(log(edspend$Population),log(edspend$Spending),col="red",xlab="Log Population",ylab="Log Spending") #plot logs
abline(lm(log(edspend$Spending)~log(edspend$Population)))
# we use logs to scale down the relationship  to visually show relationships. Log() will exaggerate outliers.
plot(edspend$Population,edspend$Spending,col="red",xlab="Log Population",ylab="Log Spending",log="xy") #plot in log scale

#2.7

canfuel<-read.csv(file.choose()) #EG02-07CANFUEL
plot(canfuel$HwyMPG,canfuel$CO2,xlab="Hwy",ylab="Emissions") #pool all types of fuels
lines(smooth.spline(canfuel$HwyMPG,canfuel$CO2,spar = 2/3))

#Different Types of fuels D, E, X, Z
plot(canfuel[canfuel$FUEL=="D",]$HwyMPG,canfuel[canfuel$FUEL=="D",]$CO2,
     xlab="Hwy",ylab="Emissions",col="blue",xlim=c(15,75), ylim=c(0,450))
par(new=TRUE) # plot on top of a plot to show the relationship of the previous plot
plot(canfuel[canfuel$FUEL=="E",]$HwyMPG,canfuel[canfuel$FUEL=="E",]$CO2,
     xlab="Hwy",ylab="Emissions",col="red",xlim=c(15,75), ylim=c(0,450))
par(new=TRUE)
plot(canfuel[canfuel$FUEL=="X",]$HwyMPG,canfuel[canfuel$FUEL=="X",]$CO2,
     xlab="Hwy",ylab="Emissions",col="green",xlim=c(15,75), ylim=c(0,450))
par(new=TRUE)
plot(canfuel[canfuel$FUEL=="Z",]$HwyMPG,canfuel[canfuel$FUEL=="Z",]$CO2,
     xlab="Hwy",ylab="Emissions",col="purple",xlim=c(15,75), ylim=c(0,450))
legend(65,450,legend=c('D','E','X','Z'), col=c('blue', 'red','green','purple'),pch=c(1,1,1,1))

lines(smooth.spline(canfuel[canfuel$FUEL=="D",]$HwyMPG,canfuel[canfuel$FUEL=="D",]$CO2,spar = 2/3),col="blue",lwd=3)
lines(smooth.spline(canfuel[canfuel$FUEL=="E",]$HwyMPG,canfuel[canfuel$FUEL=="E",]$CO2,spar = 2/3),col="red",lwd=3)
lines(smooth.spline(canfuel[canfuel$FUEL=="X",]$HwyMPG,canfuel[canfuel$FUEL=="X",]$CO2,spar = 2/3),col="green",lwd=3)
lines(smooth.spline(canfuel[canfuel$FUEL=="Z",]$HwyMPG,canfuel[canfuel$FUEL=="Z",]$CO2,spar = 2/3),col="purple",lwd=3)
lines(smooth.spline(canfuel$HwyMPG,canfuel$CO2,spar = 2/3),lwd=3) #lwd=thickness of lines
#use a smooth line to exaggerate the differences and similarities
#2.37
canfuel<-read.csv(file.choose()) #EG02-07CANFUEL
plot(canfuel$CityMPG,canfuel$HwyMPG,xlab="City MPG",ylab="Highway MPG")
(1/(length(canfuel$CityMPG)-1))*sum((canfuel$CityMPG-mean(canfuel$CityMPG))/sd(canfuel$CityMPG)*
                                      (canfuel$HwyMPG-mean(canfuel$HwyMPG))/sd(canfuel$HwyMPG)) #manually calculate correlation
cor(canfuel$CityMPG,canfuel$HwyMPG) #use correlation function
#correlations only matters when you have a linear relationship
#2.11

finmark<-read.csv(file.choose()) #EG02-09FINMARK
b1<-cor(finmark$PerCapitaGDP,finmark$AssetsPerPerson)*(sd(finmark$AssetsPerPerson)/sd(finmark$PerCapitaGDP)) #manual slope
b0<-mean(finmark$AssetsPerPerson)-b1*mean(finmark$PerCapitaGDP) #manual intercept
model <- lm(finmark$AssetsPerPerson~finmark$PerCapitaGDP) #use lm function
coef(model) #check coefficient values
plot(finmark$PerCapitaGDP,finmark$AssetsPerPerson,xlab="GDP per capita",
     ylab="Net assets per capita",col="red",ylim = c(100,400),xlim=c(30,70))
abline(a=coef(model)[[1]],b=coef(model)[[2]])

50*coef(model)[[2]]+coef(model)[[1]] #predict y for 50,000 usd

r2<-sum((model$fitted.values-mean(finmark$AssetsPerPerson))^2)/sum((finmark$AssetsPerPerson-mean(finmark$AssetsPerPerson))^2)
cor(finmark$PerCapitaGDP,finmark$AssetsPerPerson)^2 # same thing as line 67, just use the premade function for brevity
str(summary(model))


#2.16

edspend<-read.csv(file.choose()) #EG02-04EDSPEND
plot(edspend$Population,edspend$Spending,col="red",xlab="Population",ylab="Spending")
linearmodel<-lm(edspend$Spending ~ edspend$Population) #linear model: intercept and slope parameters
abline(linearmodel$coefficients[[1]],linearmodel$coefficients[[2]])

resid<-edspend$Spending - linearmodel$fitted.values #calculate residuals
plot(edspend$Population,resid,xlab="Population",ylab="Residual",col="red") #plot residuals
abline(h=0,col="red",lwd=3)

edspend$State[order(edspend$Spending, decreasing=TRUE)[1:4]] #find spenders
plot(edspend$Population,edspend$Spending,col="red",xlab="Population",ylab="Spending")
abline(linearmodel$coefficients[[1]],linearmodel$coefficients[[2]])
text(edspend$Population,
     edspend$Spending,edspend$State,cex = 0.65, pos=1,col="black") #state names

edspend$State[order(abs(edspend$resid), decreasing=TRUE)[1:4]] #find residuals 
plot(edspend$Population,edspend$resid,xlab="Population",ylab="Residual",col="red",ylim=c(-18,16)) #plot residuals
abline(h=0,col="red",lwd=3)
text(edspend$Population,
     edspend$resid,edspend$State,cex = 0.8, pos=1,col="black") #state names

qqnorm(resid) #are residuals normally distributed
hist(resid,n=20)
lines(density(resid),col=2)


#2.113
agegen<-read.csv(file.choose()) #EX02-113AGEGEN
table_fem<-xtabs(Count~Age+MaritalStatus, agegen[agegen$Gender=="Female",])
addmargins(table_fem) #sum of entries in each column
#Marginal Distributions
addmargins(table_fem)[1,5]/addmargins(table_fem)[5,5] #18-24
addmargins(table_fem)[2,5]/addmargins(table_fem)[5,5] #25-39
addmargins(table_fem)[3,5]/addmargins(table_fem)[5,5] #40-64
addmargins(table_fem)[4,5]/addmargins(table_fem)[5,5] #65-over

addmargins(table_fem)[5,1]/addmargins(table_fem)[5,5] #Divorced
addmargins(table_fem)[5,2]/addmargins(table_fem)[5,5] #Married
addmargins(table_fem)[5,3]/addmargins(table_fem)[5,5] #Never Married
addmargins(table_fem)[5,4]/addmargins(table_fem)[5,5] #Widowed

#Conditional Distribution
addmargins(table_fem)[1,1]/addmargins(table_fem)[1,5] #Given 18-24, Divorced
addmargins(table_fem)[1,2]/addmargins(table_fem)[1,5] #Given 18-24, Married
addmargins(table_fem)[1,3]/addmargins(table_fem)[1,5] #Given 18-24, NeverMarried
addmargins(table_fem)[1,4]/addmargins(table_fem)[1,5] #Given 18-24, Widowed


