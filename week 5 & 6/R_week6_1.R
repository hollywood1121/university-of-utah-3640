# assignment 5 second question. review these problems


#7.1
smrtphn<-read.csv(file.choose())
#because we don't have any idea of the population if its standard distributed, use qqnorm.
qqnorm(smrtphn$time_min)

# qqnorm looks good.

#mean of all values
mean(smrtphn$time_min)

# sd of all values given
sd(smrtphn$time_min)

# use t distribution. 97.5% all observations. T scores are used for sample data.
# qt is a distribution function, while the nrow is just a fancy was of getting n.
# instead of using nrow you can also use length(smrtphn$time_min) - 1 for the degree of freedom.
 tscore <- qt(0.975,nrow(smrtphn)-1)

# with the t* val from qt being 2.3646, you can calculate the margin of error by:



# the formula for CI, confidence level: z-score multiply (sigma divided by square root n), n being total count of sample size
ci <- tscore * sd(smrtphn$time_min) / sqrt(length(smrtphn$time_min))


# sample mean plus T value * SD of the sample divided by sq rt of all observations.
mean(smrtphn$time_min)+qt(0.975,nrow(smrtphn)-1)*sd(smrtphn$time_min)/sqrt(nrow(smrtphn))

# sample mean minus T value * SD of the sample divided by sq rt of all observations.
mean(smrtphn$time_min)-qt(0.975,nrow(smrtphn)-1)*sd(smrtphn$time_min)/sqrt(nrow(smrtphn))

#we are 95% confident that the average amount of time per day a student at your institution
#spends on their smartphone is between 88.33 and 130.7 mins

# you can use this concept to create the confidence interval. line 14 and 15. aka what the data is centered around.
















# can the results of the question 1 be generalized to the population of students at your institution? 
# To help answer this, we can use the SRS in Example 7.1 (page 361) to test whether the average time
# using a smartphone at your institution differs from the UK average of 119 minutes.



# Specifically, we want to test null hypothesis H0: \mu (µ)=119 Ha: \mu (µ) ~= 119, alpha=0.05


# example 7.2 H0: \mu=119 , Ha: \mu ~= 119, alpha=0.05

# use simple random sample to see if null population of 119 is right.

# null population is 119 because its saying the average British user does 119 mins per day


# calc t distribution to test null hypothesis. this is found by the following formula:
# sample mean - given mean divided by SD / n

sampleTdist <- (mean(smrtphn$time_min)-119) /(sd(smrtphn$time_min)/sqrt(nrow(smrtphn)))

# -1.06 see screenshot wa1

# out of curiousity run the qt. the point below 2.5, this occurs.
qt(0.975, nrow(smrtphn)-1)



pt(0.95,7)
#.8189 , pretty large


# make the P value. you can compare this to alpha. 
# 2 comes from it being a two sided test, multiplied by the PT() function
# 7 = degrees of freedom. this is found by n-1, in our case 8-1 = 7.


 pval <- 2 *pt(sampleTdist,7)

# now we have P value to compare against alpha (aka significance level)

 # if P value is equal to or less than alpha:  we reject H0
 # if P value is greater than alpha: we fail to reject H0
 
# with our case of p value being .323 and alpha being .05
# we will fail to reject H0.
 


 

#7.34 pg 377

# check scores before and after test.
# null is no difference, any thing else is an increase.
spnish<-read.csv(file.choose())


# state hypothesis. we're hoping that after taking the test, your scores are better.
  # H0 is what you hope is not true. the evidence against it to prove Ha.
 # H0: µ = 27.3 (this is the spanish pretest mean) , Ha: µ > 27.3 


#find the difference 
spnish$dif<-spnish$Posttest-spnish$Pretest

#check the distrubution, seems pretty normal.
qqnorm(spnish$dif)
mean(spnish$dif)

# produce T statistic based on sample. this is creating the t sample 
(mean(spnish$dif)-0)/(sd(spnish$dif)/sqrt(nrow(spnish)))

# going with .95 because its a one sided test. this is
qt(0.95,nrow(spnish)-1)

#the point of comparing the two is to understand the positioning.
# the sample its clearly out of tht 95% confidence interval.


# looks like the T statistic is bigger than the qt

#refer to wa2 for explanation. green value is what we dont' want since we're trying to test if the red side is showing.
1-pt((mean(spnish$dif)-0)/(sd(spnish$dif)/sqrt(nrow(spnish))),nrow(spnish)-1) #The null hypothesis is rejected at the 5% level


1-pt((mean(spnish$dif)-0)/(sd(spnish$dif)/sqrt(nrow(spnish))),nrow(spnish)-1) > 0.05
1-pt((mean(spnish$dif)-0)/(sd(spnish$dif)/sqrt(nrow(spnish))),nrow(spnish)-1) > 0.1

# update the code look at update.png

# meaning its an unlikely event that the null hypothesis =0, (no change)
# needs larger data set

mean(spnish$dif)+qt(0.95, nrow(spnish)-1) *sd(spnish$dif) /sqrt(nrow(spnish))

mean(spnish$dif)-qt(0.95, nrow(spnish)-1) *sd(spnish$dif) /sqrt(nrow(spnish))


#7.52 p 394


# The "misery is not miserly" phenomenon refers to a sad person's spending judgment going haywire. In a recent study,
# 31 young adults were given $10 and randomly assigned to either a sad or a neutral group. The participants in 
# the sad group watched a video about the death of a boy's mentor (from The Champ), and those in the neutral group 
# watched a video on the Great Barrier Reef. After the video, each participant was offered the chance to trade $0.50 
# increments of the $10 for an insulated water bottle. answer the questions:



# (a) Examine each group's prices graphically. Is use of the t procedures appropriate for these data? Carefully explain your answer.
# (b) Make a table with the sample size, mean, and standard deviation for each of the two groups.
# (c) State appropriate null and alternative hypotheses for comparing these two groups.
# (d) Perform the significance test at the ?? = 0.05 level, making sure to report the test statistic, degrees of freedom, and P-value. What is your conclusion?
# (e) Construct a 95% confidence interval for the mean difference in purchase price between the two groups







sadness<-read.csv(file.choose())

# (a) Examine each group's prices graphically. Is use of the t procedures appropriate for these data? Carefully explain your answer.
# when looking at the histogram, its skewed. lets look with some other ways.
hist(sadness$Price)

#specifying the sad group and putting into a qqnorm shows a relatively normal distribution

hist(sadness[sadness$Group=="S",]$Price,n=10, main = "Sad Group")
qqnorm(sadness[sadness$Group=="S",]$Price,n=10, main = "Sad Group")

#specifying the neutral group and putting into a qqnorm shows a relatively normal distribution

hist(sadness[sadness$Group=="N",]$Price,n=10, main = "Neutral Group")
qqnorm(sadness[sadness$Group=="N",]$Price,n=10, main = "Neutral Group")


# (b) Make a table with the sample size, mean, and standard deviation for each of the two groups.

# finding n for both groups
n2<-length(sadness[sadness$Group=="S",]$Price)
n1<-length(sadness[sadness$Group=="N",]$Price)

# finding mean for both groups
m2<-mean(sadness[sadness$Group=="S",]$Price)
m1<-mean(sadness[sadness$Group=="N",]$Price)

# finding sd for both groups
sd2<-sd(sadness[sadness$Group=="S",]$Price)
sd1<-sd(sadness[sadness$Group=="N",]$Price)


# (c) State appropriate null and alternative hypotheses for comparing these two groups.

# see if significant difference (this is a confidence interval formula for 2 sample)
twoci <- (m1-m2)/sqrt(sd1^2/n1+sd2^2/n2)

# -4.3 

 
# (d) Perform the significance test at the ?? = 0.05 level, making sure to report the test statistic, degrees of freedom, and P-value. What is your conclusion?


# t statistic given the degrees of freedom. we use n1 because we always use the lowest n value.
#this is a very small chance.

# we use 
pt(twoci,n1-1)
pt(twoci,n1-1) > 0.05 
#Reject H0: The data show evidence of a significant difference between the groups in purchase price

# (e) Construct a 95% confidence interval for the mean difference in purchase price between the two groups

# confidence interval for one sided test
m1-m2 + qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
m1-m2 - qt(.95,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)


# confidence interval for two sided test
m1-m2 + qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)
m1-m2 - qt(.975,n1-1)*sqrt(sd1^2/n1+sd2^2/n2)

