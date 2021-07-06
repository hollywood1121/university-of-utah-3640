



###############################################################################################

##### Question 1 final exam

# Get the data "EX07-55SEEDCNT2" into the Global Environment in R. As part of the Six Sigma quality
# improvement effort, the company wants to compare scoops of seeds from two different packaging plants. 
# As SRS of 50 one-pound scoops of seeds was collected from Plant 1746, and an SRS of19 one-pound scoops 
# of seeds was collected from Plant 1748.  The number of seeds in each scoop were recorded.
# a) Using this data set, create a histogram, boxplot, and Normal quantile plotof the seed counts 
# from  Plant  1746. Do the same for Plant  1748. Are the distributions reasonably Normal? 
# Is the t procedure appropriate given your observation?
# b) Construct a 99% confidence interval around mean differences of two plants
# (Hint: use the smaller sample size for calculating the degrees of freedom) (Hint: Rweek11, 7.52)
###############################################################################################

# step 1 import
seeds <- read.csv(file.choose())


# find n, mean, and sd for groups to be used for p / z / t scores
# finding n for both groups
n2<-length(seeds[seeds$Plant =="1746",]$SeedCount) # 1746 n
n1<-length(seeds[seeds$Plant =="1748",]$SeedCount) # 1748 n

# n2 = 50
# n1 = 19


# finding mean for both groups
m2<-mean(seeds[seeds$Plant =="1746",]$SeedCount) # 1746 mean
m1<-mean(seeds[seeds$Plant =="1748",]$SeedCount) # 1748 mean



# finding sd for both groups
sd2<-sd(seeds[seeds$Plant =="1746",]$SeedCount) # 1746 sd
sd1<-sd(seeds[seeds$Plant =="1748",]$SeedCount) # 1748 sd






# a) Using this data set, create a histogram, box plot, and Normal quantile plot of the seed counts 
# from  Plant  1746. Do the same for Plant  1748. Are the distributions reasonably Normal? 
# Is the t procedure appropriate given your observation?




hist(seeds[seeds$Plant =="1746",]$SeedCount, n = 5)

plot(logseeds[seeds$Plant =="1746",]$SeedCount)

plot(density(seeds[seeds$Plant =="1746",]$SeedCount))

qqnorm(seeds[seeds$Plant =="1746",]$SeedCount)

qqline(seeds[seeds$Plant =="1746",]$SeedCount, col ="steelblue", lwd=2)


boxplot(seeds[seeds$Plant =="1746",]$SeedCount)

# looking at how both boxplots compare.
boxplot(seeds[seeds$Plant =="1746",]$SeedCount, seeds[seeds$Plant =="1748",]$SeedCount, outline = FALSE)


# after examining all the graphs and tests above, it is fair to say they are about reasonably normal.

#however i have to note that the plot of density shows that the distribution looks somewhat bimodal too.

# the qqnorm helped the most. its rough, but reasonably normal.




hist(seeds[seeds$Plant =="1748",]$SeedCount, n = 5)

plot(density(seeds[seeds$Plant =="1748",]$SeedCount))

qqnorm(seeds[seeds$Plant =="1748",]$SeedCount)

qqline(seeds[seeds$Plant =="1748",]$SeedCount, col ="steelblue", lwd=2)

boxplot(seeds[seeds$Plant =="1748",]$SeedCount)

# 1748 histogram has a skewed left look, but still relatively normal. Ther are clearly big gaps within the seedcounts when n > 5.
# the qqplot does not look normal, which is making this a hard decision.


# Is the t procedure appropriate given your observation?


# this is honestly really hard to call. I can manipulate the bins being showed on a histogram for 1748 to show just a left skew but 

# at the same time the qqnorm was really out of whack. I will say that for the purpose of this exercise it "can be interpreted" as relatively normal.
# it would be wise to say that the sample size for 1749 is only 19, which i would argue is small and is a possible issue for such variance.

# T procedures are to be used if we're assuming the data sets are relatively normal will be acceptable.
# if we said that the distribution was not normal enough, then we would not use the T procedures.






# b) Construct a 99% confidence interval around mean differences of two plants
# (Hint: use the smaller sample size for calculating the degrees of freedom) (Hint: Rweek11, 7.52)



# see if significant difference t statistic
twoci <- (m1-m2)/sqrt(sd1^2/n1+sd2^2/n2)



# t statistic given the degrees of freedom as n1 - 1. we use n1 because we always use the lowest n value 
pval <-pt(twoci,n1-1)
pval > 0.05   # check if p value is bigger than alpha at 5%
pval > 0.01   # check if p value is bigger than alpha at 1%


# t = sample mean - µ0 divided by sample sd  divided by sq rt n

# confidence interval for one sided test @ 95%
m1-m2 + qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
m1-m2 - qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)




# confidence interval for two sided test @ 97.5%
m1-m2 + qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
m1-m2 - qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)


# confidence interval for two sided test @ 99%
up3 <- m1-m2 + qt(.99,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
up4 <- m1-m2 - qt(.99,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)

# because the difference in sample size is showing. the margin of Error
# and the variance is notable. 







###############################################################################################

##### Question 2 final exam

# Get the data "EX07-57DRVTHRU" into the Global Environment in R. QSRMagazine.com assessed 
# 1855 drive-thru visits at quick service restaurants. One benchmark assessed was customer service.
# Responses ranged from 'Rude (1)' to 'Very Friendly (5)'. The data set breaks down the responses
# according to two of the chains studied. 

# a)	A researcher decides to compare the average rating of McDonald's and Taco Bell. Assuming an 
# average of these ratings makes sense, report the means and standard deviations # of the ratings 
# for each chain separately.

# b)	Test whether the two chains, on average, have the same customer satisfaction. Use a 
# two-sided alternative hypothesis and a signi???cance level of 5%. (Hint: R week11, 7.52)

###############################################################################################

# step 1 import
Dthru <- read.csv(file.choose())

# a)	A researcher decides to compare the average rating of McDonald's and Taco Bell. Assuming an 
# average of these ratings makes sense, report the means and standard deviations # of the ratings 
# for each chain separately.


# find n, mean, and sd for groups to be used for p / z / t scores
# finding n for both groups
n2<-length(Dthru[Dthru$Chain =="McDonald's",]$VisitScore) # Mcdonalds n
n1<-length(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore) # Taco Bell n

# n2 = 317
# n1 = 308


# finding mean for both groups
m2<-mean(Dthru[Dthru$Chain =="McDonald's",]$VisitScore) # Mcdonalds mean
m1<-mean(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore) # Taco Bell mean



# finding sd for both groups
sd2<-sd(Dthru[Dthru$Chain =="McDonald's",]$VisitScore) # Mcdonalds sd
sd1<-sd(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore) # Taco Bell sd


# b)	Test whether the two chains, on average, have the same customer satisfaction. Use a 
# two-sided alternative hypothesis and a signi???cance level of 5%. (Hint: R week11, 7.52)


# state hypothesis. 
# H0 is what you hope is not true, H0 is the no change value. the evidence against it to prove Ha.

# we want to test whether two chains, on average, have the same customer satisfaction. 

# H0: mcdonald's mean satisfaction (µ)  = Taco bell's mean satisfaction (µ)  thus, Ha: mcdonald's mean satisfaction (µ) !=  Taco bell's mean satisfaction  (µ) 



# see if significant difference  t statistic
twoci <- (m1-m2)/sqrt((sd1^2/n1) + (sd2^2/n2))



# t statistic given the degrees of freedom as n1 - 1. we use n1 because we always use the lowest n value 
pval <-2*pt(twoci,n1-1)
pval2 <-2*pt(twoci,n2-1)



pval > 0.05   # check if p value is bigger than alpha at 5%
pval > 0.01   # check if p value is bigger than alpha at 1%

pval2 > 0.05   # check if p value is bigger than alpha at 5%
pval2 > 0.01   # check if p value is bigger than alpha at 1%
# pval > 0.05 = true, that means we fail to reject H0. 
# pval2 > 0.05 = true, that means we fail to reject H0. 






###############################################################################################

##### Question 3 final exam

# Get the data "Ex07-73WHEAT" into the Global Environment in R. A quick survey data which collected 
# prices from only ???ve producers each month is presented. Assume that we will perform a two-sided 
# test using the 5% signi???cance level. Find the critical value for the unpooled t-test statistic 
# that does not assume equal variances. Use the minimum of n1 ??? 1 and n2 ??? 1 for the degrees of freedom. 
# (Hint: Example 7.13 "More about wheat prices" in your textbook)

###############################################################################################


# step 1 import
wheatinfo <- read.csv(file.choose())


# find n, mean, and sd for groups to be used for p / z / t scores
# finding n for both groups
n2<-length(wheatinfo[wheatinfo$Month =="January",]$Price) # January n
n1<-length(wheatinfo[wheatinfo$Month =="July",]$Price) # July n

# n2 = 5
# n1 = 5


# finding mean for both groups
m2<-mean(wheatinfo[wheatinfo$Month =="January",]$Price) # January mean
m1<-mean(wheatinfo[wheatinfo$Month =="July",]$Price) # July mean



# finding sd for both groups
sd2<-sd(wheatinfo[wheatinfo$Month =="January",]$Price) # January sd
sd1<-sd(wheatinfo[wheatinfo$Month =="July",]$Price) # July sd




# see if significant difference  t statistic
twoci <- (m1-m2)/sqrt((sd1^2/n1) + (sd2^2/n2))

pval <-pt(twoci,n1-1)
pval2 <-pt(twoci,n2-1)



pval > 0.05   # check if p value is bigger than alpha at 5%
pval > 0.01   # check if p value is bigger than alpha at 1%


pval2 > 0.05   # check if p value is bigger than alpha at 5%
pval2 > 0.01   # check if p value is bigger than alpha at 1%

# pval > 0.05 = true, that means we fail to reject H0. 

# pval2 > 0.05 = true, that means we fail to reject H0. 




