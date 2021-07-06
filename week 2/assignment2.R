setwd("c:\\Users\\nicka\\Desktop\\econ3640")
#colors 
install.packages('viridis')
library(viridis)
# disabling population numbers to show better for question 2
# options(scipen = 999)
###############################################################################################

##### Question 1

# Get the dataset "EX02-13BEER" into the Global Environment in R, make a scatterplot of calories
# versus percent alcohol. Calculate the correlation between calories and percent alcohol. 
# Use a linear model describe the relationship between calories and percent alcohol, what are 
# the coefficient values? (Hint: R script "R week2-1", questions 2.4 and 2.37.
# The dataset has empty cells, you may want to drop the rows with empty cells)

###############################################################################################


# growth<-growth[complete.cases(growth),] #drop NAs

beer<-read.csv(file.choose()) #EG02-13BEER

#drop cases of N/A
beer<-beer[complete.cases(beer),] #drop NAs


#plot the data to see if there's any correlation to be made visually
plot(beer$CaloriesPer12Oz ,beer$PercentAlcohol ,col="red",xlab="Calories Per 12 Ounces",ylab="Percentage of Alcohol", sub="A relationship exists in this data set which shows the more calories a beer has, the higher the alcohol percentage there is.", main="Postive Relationship of calories in beer and % in alcohol")


#linear model: intercept and slope parameters
linearmodel<-lm(beer$PercentAlcohol ~ beer$CaloriesPer12Oz) 


#store the coefficients
coef<-linearmodel$coefficients

# Returns a correlation coeff .9047062 which can be seen pretty visually
abline(coef[1],coef[2]) #plot linear model

#alternatively we can also run the cor() function to determine if there is a correlation
cor.test(beer$PercentAlcohol, beer$CaloriesPer12Oz, method="pearson")




print(coef)

#using the lineal model function we were able to take the coefficient values as
  # (Intercept)       beer$CaloriesPer12Oz 
  # 0.0076558566         0.0002885993 



###############################################################################################

##### Question 2

# Get the dataset "EX02-13BEER" into the Global Environment in R and use a linear model describe
# the relationship between calories and percent alcohol. Plot the residuals from the linear model
# and a horizontal line at 0, then, plot the histogram of the residuals, do they 
# look normally distributed? (Hint: R script "R week2-1", question 2.16)

###############################################################################################

beer<-read.csv(file.choose()) #EG02-13BEER

#drop cases of N/A
beer<-beer[complete.cases(beer),] #drop NAs


#plot the data to see if there's any correlation to be made visually
plot(beer$CaloriesPer12Oz ,beer$PercentAlcohol , col="red", xlab="Calories Per 12 Ounces", ylab="Percentage of Alcohol", sub="A relationship exists in this data set which shows the more calories a beer has, the higher the alcohol percentage there is.", main="Postive Relationship of calories in beer and % in alcohol")

#alternatively we can also run the cor() function to determine if there is a correlation
cor.test(beer$PercentAlcohol, beer$CaloriesPer12Oz, method="pearson")

#convert into linear model to grab coefficients
linearmodel<-lm(beer$PercentAlcohol ~ beer$CaloriesPer12Oz) 

#store the coefficients
coef<-linearmodel$coefficients

#create a line to show thew trends
abline(linearmodel$coefficients[[1]],linearmodel$coefficients[[2]])

#create the residuals by finding the difference between percent in alcohol vs the fitted values
resid <- beer$PercentAlcohol - linearmodel$fitted.values

#plot the residual data to see if it's a normal distribution
plot(beer$CaloriesPer12Oz ,resid,xlab="calories per 12 Oz",ylab="Residual",col="red") #plot residuals

#create the horizontal line at 0 
abline(h=0,col="red",lwd=3)

#it's hard to tell with the previous plot, so we're going to put it into a histogram
hist(resid,col = viridis(10), n=20)

#  looks approximately normal, I put up a line to see the distribution.
lines(density(resid),col="red", lwd=2)

# I did a qqnorm test as well to see the distribution and when drawing a line through it, it seems approximately normal.
qqnorm(resid, main ="distribution of residuals ")
qqline(resid, col="red")



###############################################################################################

##### Question 3

# Get the dataset "EX02-13BEER" into the Global Environment in R and make a scatterplot of calories
# versus percent alcohol. Find the brand "O'Doul's" from the brewery "Anheuser Busch" in your dataset.
# Put a name on top of "O'Doul's" in your plot (x axis 0.005 y axis 95 should work as location).
# Remove "O'Doul's" from your dataset and use a linear model again, create a scatterplot with "O'Doul's"
# included and plot both the model with "O'Doul's" (the model you created in Question-1) and the model without 
# "O'Doul's", did "O'Doul's" make a big difference? (Hint: R script "R week2-1", question 2.16)


###############################################################################################

# read it in
beer<-read.csv(file.choose()) #EG02-13BEER

# drop cases of N/A
beer<-beer[complete.cases(beer),] #drop NAs

# find the row of Odouls and print it out
  Odoulsline <- which(beer$Brand == "O'Doul's")
  
# you can also find by:  beer[beer$Brand=="O'Doul's",]

#  save the row of odouls to take out and put back in, create the indexes
  odouls <- beer[beer$Brand=="O'Doul's",]
  Odoulslinedeletion <- ( -1 * Odoulsline ) 

 
  
#plot the data to see if there's any correlation to be made visually
plot( beer$CaloriesPer12Oz, beer$PercentAlcohol, col="red", xlab="Calories Per 12 Ounces", ylab="Percentage of Alcohol", sub="A relationship exists in this data set which shows the more calories a beer has, the higher the alcohol percentage there is.", main="Postive Relationship of calories in beer and % in alcohol")

#plot out the line where odouls is.
text(70, 0.0015 , beer$Brand[Odoulsline] , cex = 0.8 ,col="black") 

# Remove "O'Doul's" from your dataset and use a linear model again
beer <- beer[Odoulslinedeletion,]

# with "O'Doul's" gone, plot the data to see if there's any correlation to be made visually
plot( beer$CaloriesPer12Oz, beer$PercentAlcohol, col="red", xlab="Calories Per 12 Ounces", ylab="Percentage of Alcohol", sub="A relationship exists in this data set which shows the more calories a beer has, the higher the alcohol percentage there is.", main="Postive Relationship of calories in beer and % in alcohol")

#alternatively we can also run the cor() function to determine if there is a correlation
cor.test(beer$PercentAlcohol, beer$CaloriesPer12Oz, method="pearson")

#there is more of a correlation at .907

#convert into linear model to grab coefficients
linearmodel<-lm(beer$PercentAlcohol ~ beer$CaloriesPer12Oz) 

#store the coefficients
coef<-linearmodel$coefficients

#create a line to show thew trends
abline(linearmodel$coefficients[[1]],linearmodel$coefficients[[2]])


# when looking at the differences between when odouls was included and not included, 
# it was very obvious that it was different. the slope of the AB line was different
# and it clearly changed the way the graph presented its starting point (close to origin).


###############################################################################################

##### Question 4

# Get the dataset "EX02-111HIRING" into the Global Environment in R. This data belongs 
# to a company accused of age discrimination in hiring for operator positions; 
# data describes applications for the past three years. Use the xtab() function 
# to create a table "Age" being a row variable, "Hired" being a column variable. 
# Next, use the function addmargins() to add the sums as a new row and a new column.

# Using the extended table, calculate the conditional distributions of hired/not hired
# -one for applicants who are less than 40 years old and one for applications who are not
# less than 40 years old. Use a bargraph to compare these ratios, describe the hiring record
# in words, can you conclude based on these results there is evidence on discrimination?
#  (Hint: R script "R week2-1", question 2.113)


###############################################################################################


# read in hiring 
hiringinfo<-read.csv(file.choose()) #EX02-111HIRING

# use xtab funcion to create table age being a row variable, hired is a column variable
# use addmargins() to create sums of new rows

# the first table made is the under 40, comparing the age groups and being hired.
# this will also include addmargins saved in the variable as we need to tweak
table_under40<- addmargins(xtabs(Count~Age+Hired, hiringinfo[hiringinfo$Age=="YoungerThan40",]))
chart_under40 <- xtabs(Count~Age+Hired, hiringinfo[hiringinfo$Age=="YoungerThan40",])
# create a percentage that shows the relation of distribution of hired / not hired to understand
# if there was / wasn't a bias on the under 40 vs over 40.
# divide the number of hired people into the amount of sum of all to understand the conditional distributions.
percentageofEmployedUnder40 <- (table_under40[1,1] / table_under40[1,3]) * 100 


# using addmargins, we will find the conditional distributions of hired / not hired people who are older than 40.

table_over40<- addmargins(xtabs(Count~Age+Hired, hiringinfo[hiringinfo$Age=="40OrOlder",]))
percentageofEmployedOver40 <- (table_over40[1,1] / table_over40[1,3]) * 100 
chart_over40 <- xtabs(Count~Age+Hired, hiringinfo[hiringinfo$Age=="40OrOlder",])
#Use a bargraph to compare these ratios

# establish a dataframe that links the percentage of each age segment
comparison <- data.frame(percentageofEmployedOver40, percentageofEmployedUnder40)

# these two barplots show the relative amount of workers highered within their age group 
# to get an idea of the general percentage of higherings.
barplot(chart_under40, col =viridis(10), xlab = "Amount of workers", ylab = "Total amount of applicants", main = "Hiring percentages of people under 40")
barplot(table_under40, col =viridis(10), xlab = "Amount of workers", ylab = "Total amount of applicants", main = "Hiring percentages of people under 40")

barplot(chart_over40, col =viridis(10), xlab = "Amount of workers", ylab = "Total amount of applicants", main = "Hiring percentages of people over 40")
barplot(table_over40, col =viridis(10), xlab = "Amount of workers", ylab = "Total amount of applicants", main = "Hiring percentages of people under 40")

# this barplot shows the comparison of the percentages of the two segments put together,
# to show that employees that are under 40 still are highered almost 6x as often
barplot(as.matrix(comparison), col =viridis(10), ylab = "percentage of hired applicants from their age group", main = "Comparison of under and over 40 workers hiring rates")


# use words to describe the how the company hires:

# when looking at the percentage of people under 40 versus over 40, I notice that the 40OrOlder applicants
# sum is 170 versus the YoungerThan40 which is 1242. This shows us that over the last 3 years, more young
# people tend to apply which would also indicate that young people have a majority. Thus, it is possible
# after comparing percentage ratios to see that YoungerThan40 has a 6.6% success rate of hiring 
# versus 40OrOlder has a 1.17%  percent success rate. While there are other factors that can indicate why the 
# ratio for 40OrOlder is lower (e.g. hardskill test failed, team interview fit, etc), 
# if we were judging this data alone and forced to make a decision, 
# it is possible to say that there could definitely be a bias in hiring procedures.



