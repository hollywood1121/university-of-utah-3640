


###############################################################################################

##### Question 1

# Get the dataset "ex06-128ODOR" into the Global Environment in R. Many food products contain 
# small quantities of substances that would give an undesirable taste or smell if they were 
# present in large amounts. Wine experts have determined the odor threshold, the lowest 
# concentration of a compound that the human nose can detect. The odor threshold for 
# dimethylsul???de (DMS) is given as 25 micrograms per liter of wine (µg/l). Assume that 
# the standard deviation of the odor threshold for untrained noses is known to be ?? = 7µg/l.
# Obtain 95% con???dence interval for the mean DMS (Hint: R script "R week5 1", question 6.44)

###############################################################################################


#install.packages("viridis")
#library(viridis)


# step 1 import "ex06-128ODOR" 
odor<-read.csv(file.choose()) 

# step 2 determine the threshold at 25 (µg/l) with sigma ?? = 7 (µg/l) and n, n being total count of sample size

nodor <- length(odor$DMS) # n, total count of sample size
samplemean <- mean(odor$DMS) #  mean of sample 
sigmaodor <- 7 # sigma of population, which we can use for the sample.

# step 3 find the confidence interval of 95% given the mean and sigma using proper formulas

# TWO SIDED TESTS USUALLY USE A QNORM OF 0.975 or 97.5%
# ONE SIDED TESTS USUALLY USE A QNORM OF 0.95 or 95%
zscoreodor <- qnorm(0.95,0,1) #to find the z* with a given CI in a two sided test

#step 4 margin of error for the mean estimate

# the formula for CI, confidence level: z-score multiply (sigma divided by square root n), n being total count of sample size
ciodor <- zscoreodor * sigmaodor / sqrt(nodor)
  

# when multiplying the mean of the population (??) with the plus minus z-score * ( sigma divided by square root n), n being total count of sample size
# this will create the 97.5% Values along the curve. the plus will be the high number and the minus will be the normal number.

#  Obtain 95% con???dence interval for the  DMS (Hint: R script "R week5 1", question 6.44)
ci1 <- samplemean + ciodor
ci2 <- samplemean - ciodor





###############################################################################################

##### Question 2

# Get the dataset "EX07-63EGO" into the Global Environment in R. Employers sometimes seem to 
# prefer executives who appear physically fit, despite the legal troubles that may result. 
# Employers may also favor certain personality characteristics. Fitness and personality are related. 
# In one study, middle-aged college faculty who had volunteered for a fitness program were divided
# into low-fitness and high-fitness groups based on a physical examination. 
# The subjects then took the Cattell Sixteen Personality Factor Questionnaire.

# a) Is the difference in mean ego strength significant at the 5% level? At the 1% level? Be sure to state H0 and Ha.
# b)	Can you generalize these results to the population of all middle-aged men? Give reasons for your answer
# c)	Can you conclude that increasing fitness causes an increase in ego strength? Give reasons for your answer.
# (Hint: R script "R week6 1", question 7.52)
###############################################################################################

# when we don't know sigma (standard deviation) then we estimate it. with t stats.

# step 1 import
ego <- read.csv(file.choose())

# its skewed to the right but still somewhat normal
hist(ego$Ego, n=15)
# even when looking at a qqplot there are some curves up and down but overall its relatively normal
qqnorm(ego$Ego)

# to importance of how normal the distribution is directly impacts how we can answer question b.


# finding n for both groups
n2<-length(ego[ego$Group =="0",]$Fitness) # low group n
n1<-length(ego[ego$Group =="1",]$Fitness) # high group n

# finding mean for both groups
m2<-mean(ego[ego$Group =="0",]$Ego) # low group mean
m1<-mean(ego[ego$Group =="1",]$Ego) # high group mean

# finding sd for both groups
sd2<-sd(ego[ego$Group =="0",]$Ego) # low group sd
sd1<-sd(ego[ego$Group =="1",]$Ego) # high group sd

# note the following:
# the SD of the high fitness group ego is lower than the SD of the low fitness group ego.
# this means there is less deviation from the mean.
# the mean of ego for the high fitness is higher by 1.789.
# so not only do the egos of higher fitness exist but they are less likely to deviate from the high mean


# -------------------------------------------------------
# -------------------------------------------------------

# a) Is the difference in mean ego strength significant at the 5% level? At the 1% level? Be sure to state H0 and Ha.


# state hypothesis. we're hoping that after taking the test, your scores are better.
# H0 is what you hope is not true, H0 is the no change value. the evidence against it to prove Ha.

# we want to test whether Fitness and personality relate to a higher ego. 

# H0: Low fitness Ego mean (µ)  = High fitness Ego mean (µ)  thus, Ha: Low fitness Ego mean (µ)  < High fitness Ego mean (µ) 


# see if significant difference  t statistic
twoci <- (m1-m2)/sqrt(sd1^2/n1+sd2^2/n2)



# t statistic given the degrees of freedom as n1 - 1. we use n1 because we always use the lowest n value but in 
# this case n = 14 for both n1 and n2.

pval <-pt(twoci,n1-1)
pval > 0.05   # check if p value is bigger than alpha at 5%
pval > 0.01   # check if p value is bigger than alpha at 1%

# at both 5% significant and 1%, P value was bigger, therefore proving our Ha of Low fitness Ego mean (µ)  < High fitness Ego mean (µ)  


# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------


# b)	Can you generalize these results to the population of all middle-aged men? Give reasons for your answer

# to say that a group of  28 men you're able to accurately predict the whole make population seems like generalizing too much.
# similar to what we read in assignment 3 about the mind projection fallacy, glazing over details that would be considered 
# "too intricate" to deal with but in real life hold more weight than we'd like to believe.

# the sample sizes are too small to give an accurate depiction of the population. despite being somewhat normal as discussed above, I am still skeptical.
# this sample it can provide some insight. but no definitive answer. in my personal opinion. BUT!

# HOWEVER, the book and slides state that:

# 1. Sample means are less variable than individual observations.
# 2. Sample means are centered around the population mean.
# 3. Sample means are more Normal than individual observations.

# According to the book, yes in a very loose general sense the data can be generalized to the population of all middle aged men.

# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------



# t = sample mean - µ0 divided by sample sd  divided by sq rt n

# confidence interval for one sided test
up1 <- m1-m2 + qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
up2 <- m1-m2 - qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)




# confidence interval for two sided test
up3 <- m1-m2 + qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
up4 <- m1-m2 - qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)

# c)	Can you conclude that increasing fitness causes an increase in ego strength? Give reasons for your answer.

# as mentioned before, for this specific sample:

# the SD of the high fitness group ego is lower than the SD of the low fitness group ego.
# this means there is less deviation from the mean of the high fitness.
# the mean of ego for the high fitness is higher by 1.789.
# so not only do the egos of higher fitness exist but they are less likely to deviate from the high mean.

# if we are just judging this data and not taking anything else into account, you can safely assume that 
# increasing fitness has a high probability of increasing in ego strength.


# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------




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



# finding n for both groups
n2<-length(seeds[seeds$Plant =="1746",]$SeedCount) # 1746  n
n1<-length(seeds[seeds$Plant =="1748",]$SeedCount) # 1748  n


# finding mean for both groups
m2<-mean(ego[ego$Group =="0",]$Ego) # low group mean
m1<-mean(ego[ego$Group =="1",]$Ego) # high group mean

# finding sd for both groups
sd2<-sd(ego[ego$Group =="0",]$Ego) # low group sd
sd1<-sd(ego[ego$Group =="1",]$Ego) # high group sd

hist(seeds[seeds$Plant =="1746",]$SeedCount, n = 40)
qqnorm(seeds[seeds$Plant =="1746",]$SeedCount)
qqline(seeds[seeds$Plant =="1746",]$SeedCount, col ="steelblue", lwd=2)
boxplot(seeds[seeds$Plant =="1746",]$SeedCount)

shapiro.test(seeds[seeds$Plant =="1746",]$SeedCount)

hist(seeds[seeds$Plant =="1748",]$SeedCount, n = 10)
lines(density(seeds[seeds$Plant =="1748",]$SeedCount))
qqnorm(seeds[seeds$Plant =="1748",]$SeedCount)
qqline(seeds[seeds$Plant =="1748",]$SeedCount, col ="steelblue", lwd=2)
boxplot(seeds[seeds$Plant =="1748",]$SeedCount)

# 1748 has a positive skew from the boxplot perspective

# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------
# -------------------------------------------------------


# probability distribution of a continuous random variable is summarized by its probability density function (PDF)

# the cumulative distribution function (CDF) is defined  just as in the discrete case. This means the CDF of a 
# continuous random variables states the probability that the random variable is less than or equal to a particular value.


# Note that the notation X ~ Y reads as "X is distributed as Y". 

# draw a plot of the N(0,1) PDF
curve(dnorm(x),
      xlim = c(-3.5, 3.5),
      ylab = "Density",
      main = "Standard Normal Density Function")


# compute density at x=-1.96, x=0 and x=1.96 

dnorm(x = c(-1.96, 0, 1.96))

#> [1] 0.05844094 0.39894228 0.05844094





# plot the standard normal CDF 
curve(pnorm(x),
      xlim = c(-3.5, 3.5),
      ylab = "Probability",
      main = "Standard Normal Cumulative Distribution Function")

# define a vector of reals
quants <- c(-1.96, 0, 1.96)
# compute densities
f(quants)
#> [1] 0.05844094 0.39894228 0.05844094
# compare to the results produced by 'dnorm()' 
f(quants) == dnorm(quants)
#> [1] TRUE TRUE TRUE

