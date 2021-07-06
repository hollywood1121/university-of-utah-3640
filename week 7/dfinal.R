# 1.a
seeds <- read.csv(file.choose())
n2<-length(seeds[seeds$Plant =="1746",]$SeedCount) 
n1<-length(seeds[seeds$Plant =="1748",]$SeedCount) 
m2<-mean(seeds[seeds$Plant =="1746",]$SeedCount) 
m1<-mean(seeds[seeds$Plant =="1748",]$SeedCount) 
sd2<-sd(seeds[seeds$Plant =="1746",]$SeedCount) 
sd1<-sd(seeds[seeds$Plant =="1748",]$SeedCount)

hist(seeds[seeds$Plant =="1746",]$SeedCount)
boxplot(seeds[seeds$Plant =="1746",]$SeedCount)
qqnorm(seeds[seeds$Plant =="1746",]$SeedCount)
hist(seeds[seeds$Plant =="1748",]$SeedCount)
boxplot(seeds[seeds$Plant =="1748",]$SeedCount)
qqnorm(seeds[seeds$Plant =="1748",]$SeedCount)

# mostly normal, we can use t-procedure

# 1.b

(m1-m2)/sqrt(sd1^2/n1+sd2^2/n2) # t value 
pt((m1-m2)/sqrt(sd1^2/n1+sd2^2/n2),n1-1) # t value with n1 as df (smaller)


m1-m2 + qt(.99,n1-1)*sqrt(sd1^2/n1+sd2^2/n2) # 99% CI +
m1-m2 - qt(.99,n1-1)*sqrt(sd1^2/n1+sd2^2/n2) # 99% CI -


# 2.a
Dthru <- read.csv(file.choose())
n2<-length(Dthru[Dthru$Chain =="McDonald's",]$VisitScore)
n1<-length(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore)
m2<-mean(Dthru[Dthru$Chain =="McDonald's",]$VisitScore)
m1<-mean(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore)
sd2<-sd(Dthru[Dthru$Chain =="McDonald's",]$VisitScore)
sd1<-sd(Dthru[Dthru$Chain =="Taco Bell",]$VisitScore) 

# 2.b

# H0: mcdonald visitscore = Taco bell visitscore 
# Ha: mcdonald visitscore not equal Taco bell's visitscore 

(m1-m2)/sqrt(sd1^2/n1+sd2^2/n2) # t value 
pt((m1-m2)/sqrt(sd1^2/n1+sd2^2/n2),n1-1) # t value with n1 as df (smaller)
pt((m1-m2)/sqrt(sd1^2/n1+sd2^2/n2),n1-1) > .05 #significance level

#Fail to reject H0 (p>\alpha)



# 3

wheat <- read.csv(file.choose())
n2<-length(wheat[wheat$Month =="January",]$Price) 
n1<-length(wheat[wheat$Month =="July",]$Price) 
m2<-mean(wheat[wheat$Month =="January",]$Price)
m1<-mean(wheat[wheat$Month =="July",]$Price) 
sd2<-sd(wheat[wheat$Month =="January",]$Price) 
sd1<-sd(wheat[wheat$Month =="July",]$Price)

(m1-m2)/sqrt(sd1^2/n1+sd2^2/n2) # t value 
2*pt((m1-m2)/sqrt(sd1^2/n1+sd2^2/n2),n1-1) > .05 #significance level for n1
2*pt((m1-m2)/sqrt(sd1^2/n1+sd2^2/n2),n2-1) > .05 #significance level for n2
