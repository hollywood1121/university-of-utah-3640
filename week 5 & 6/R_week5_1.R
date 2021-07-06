# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44
# FOR THE QUESTION 1 ON #5 is REFERRING TO 6.44


#6.2
delta<-read.csv(file.choose())
hist(delta$Delay,n=150,prob=T)
sample(delta$Delay,100)

delay<-matrix(NA,nrow=1000,ncol=1)
for (i in 1:1000){
  delay[i,1]<-mean(sample(delta$Delay,100))
}

hist(delay,n=100)
qqnorm(delay)

#6.16
#Average Facebook user has 190 friends, takes only integer values, so definitely not normally distributed
#Highly skewed to the right, with a median of 100 friends. Suppose sigma=288, and SRS of 70 users
#sigma aka standard deviation
sigma1 <- 288
n <- 70
#a) Sample mean = 190
sigma1 /sqrt(n) #sample standard deviation
#b) use the central limit theorom to find the probability that the average number of friends for 70 users is greater than 250.
avg <- 190

# this is not using a z score. still finding the probability that the average number of friends for 70 Facebook users is greater than 250.
1-pnorm(250,avg,sigma1/sqrt(n)) #no z-score

#.04 aka 4%, pretty low.

zscore<-(250-190)/(sigma1/sqrt(70))
1-pnorm(zscore,0,1) #with z-score

#c) Average user has 190 friends, sample of 70 users
# What are the mean and standard deviation of the total number of friends in your sample?

# average user in this case is the mean of 190.
# average user * sample n = 190 * 70
# therefore in this case the mean multiplied by the average amount of friends 
n*avg


sqrt(sigma1^2*n)
#d)
1-pnorm(17500,70*190,sqrt(288^2*70)) #no z-score

zscore<-(17500-70*190)/(sqrt(288^2*70))
1-pnorm(zscore,0,1)

#6.17
firmsize<-read.csv(file.choose())
sample(firmsize$Size,3)
meansize<-data.frame(c(mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),
                       mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),
                       mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),mean(sample(firmsize$Size,3)),
                       mean(sample(firmsize$Size,3))))

#6.44
mileage<-read.csv(file.choose()) #suppose sigma=6.5

# what is the sampling distribution? xbar (the x with a bar over it) is the sample mean.
# when you take a bunch of means from different samples, they should center around the population mean.


# initially the question is asking: what is sigma xbar, the standard deviation of xbar?
############### because sigma is given so we use the standard deviation sample formula: sigma divided by square root n, n being total count of sample size

6.5/sqrt(length(mileage$MPG)) #sd of xbar



# TWO SIDED TESTS USE A QNORM OF 0.975.
# ONE SIDED TESTS USE A QNORM OF 0.95 or 95%
zscore <- qnorm(0.975,0,1) #to find the z* with a given CI in a two sided test

mean(mileage$MPG) # population parameter, mean of population.

# what is the margin of error for the mean estimate?
############### the formula for CI, confidence level: z-score multiply (sigma divided by square root n), n being total count of sample size
zscore*6.5/sqrt(20) #%95 CI margin of error

# when multiplying the mean of the population (??) with the plus minus z-score * ( sigma divided by square root n), n being total count of sample size
# this will create the 97.5% Values along the curve. the plus will be the high number and the minus will be the normal number.

# does 27MPG in between the 97.5% confidence level?
mean(mileage$MPG)+zscore*6.5/sqrt(20)
mean(mileage$MPG)-zscore*6.5/sqrt(20)

# the answer is yes, because we got 31.64 and 25.95 implying that 27 is within the confidence interval.