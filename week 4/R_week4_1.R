#4.120
#Suppose X is a random variable with mean 20 and standard deviation 5
#Suppose Y is a random variable with mean 40 and standard deviation 10
#Find the mean of the random variable Z if

# Z = 2 + 10*X ; 2+10*20 = 202
# Z = 10*X - 2 ; 10*20-2 = 198
# Z = X + Y    ; 20+40 = 60
# Z = X - Y    ; 20-40 = -20
# Z = -3*X - 2*Y ;-3*20-2*40 = -140

#4.123
#Suppose X is a random variable with mean 20 and standard deviation 5
#Suppose Y is a random variable with mean 40 and standard deviation 10
#Assume the correlation between X and Y is 0.5
#Find the variance and standard deviation of the random variable Z if

# Z = X + Y ; 5^2+10^2+2*0.5*5*10 = 175
# Z = X - Y ; 5^2+10^2-2*0.5*5*10 = 75
# Z = -3*X - 2*Y ; (-3)^2*5^2+(-2)^2*10^2-2*0.5*(-3)*(-2)*5*10 = 325

# in the above question, when you see -3, its saying it wants 
# the -3^2 (meaning 9) variants.



#4.143
wine<-read.csv(file.choose())
sum(wine) #legitimate assignment of probabilities
wine[1,1]+wine[2,2]+wine[3,3]+wine[4,4]+wine[5,5] #probability of agreeing
sum(wine[upper.tri(wine, diag = FALSE)]) #probability of taster 2 ranking higher than taster 1
sum(wine[4,]+wine[5,])

# learn about upper triangles, lower triangles, etc for probability
# check in the books. check random variables as well

#5.23

# S&P 500 index has a probability 0.56 of increasing in any week. Moreover, the change in the index
# in any given week is not influenced by whether it rose or fell in earlier weeks. Let X be the number
# of weeks among the next five weeks in which the index rises
#binomial distribution.
# p is given at 56%
# n is the number of trials total = 5
# x is from 0-5 (the first param) 
# the first number is the chance. 
# what are the chances of seeing no success at all? dbinom(0,5,0.56)
# what are the chances of seeing one week of success? dbinom(1,5,0.56)
# you get the idea
dbinom(0,5,0.56) #0 out of 5 weeks
dbinom(1,5,0.56) #1 out of 5 weeks
dbinom(2,5,0.56) #2 out of 5 weeks
dbinom(3,5,0.56) #3 out of 5 weeks
dbinom(4,5,0.56) #4 out of 5 weeks
dbinom(5,5,0.56) #5 out of 5 weeks

x<-seq(0,5,1)
y<-dbinom(x,5,0.56)

#doing type h is to turn a plot system to a bar plot.
plot(x,y,type="h",xlab="",ylab="Probability")

# the probability of four or less subtracted with 3 or less will give you the 
# same result as dbinom(4,5,0.56), because of the way the formulas using n! are built.
# the same as pbinom(4,5,0.56) - pbinom(3,5,0.56)

pbinom(4,5,0.56) #probability that 4 or less

5*0.56 #mean = n*p
sqrt(5*0.56*(1-0.56)) #sd = (n*p*(1-p))^(1/2)



#5.32 failures are seen as successes in this problem.

#Say P(X=10) = 0.106959 when X has a B(150,0.08) distribution. 
#Suppose we wish to find P(X=10) using the normal approximation
# P(X=10) if we have no continuity correction?
# P(9.5 < X < 10.5)
150*0.08 #mean
sqrt(150*0.08*(1-0.08)) #sd

# mean of  9.5 / sd 
(9.5-150*0.08)/(sqrt(150*0.08*(1-0.08))) #z-score for 9.5 = -0.7524116

# mean of  10.5 / sd 
(10.5-150*0.08)/(sqrt(150*0.08*(1-0.08))) #z-score for 10.5 = -0.4514469

#approximation found using information above
pnorm(-0.4514469,0,1) - pnorm(-0.7524116,0,1)

#5.35
# Compare the sample with population to comment on undercoverage, nonresponse and other errors
# About 13% of American adults are black. The number X of blacks in a random sample of 1500 adults should
# therefore, vary with binomial (n = 1500, p=0.13)
1500*0.13 #n*p=195 mean , on abertage, how many black americans should be in your sample (13%)

#standard deviation
sqrt(1500*0.13*(1-0.13)) #sd=13.02498

# is this a normal approximation? 10 is thresholds and is normal
1500*0.13 > 10
1500*(1-0.13) > 10
pnorm(170,195,13.02498) #sample should contain 170 or fewer black adults
# picking 170 and below

#5.46 page 274? POISSONS LOOK AT AVERAGES

# The average number of emails received by a particular employee at your company is five emails per hour
# Suppose this can be adequately modeled as a Poission random variable

# probability of receiving exactly five emails in any given hour (dpois(5,5) works as well)
ppois(5,5)-ppois(4,5) 
#you can also do dpois(5,5). because this problem is discrete, dpois works. if it wasn't discrete, use ppois.



ppois(4,5) # probability of receiving less than five emails in any given hour
1-ppois(0,5) # probability of receiving at least one mail in any given hour

#because normally its 5 emails per hour, average for 30 mins is 2.5
1-ppois(0,2.5) # probability of receiving at least one mail in any given 30-minute span
