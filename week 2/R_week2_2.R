#3.8

install.packages("dplyr")
library(dplyr)
brands<-read.csv(file.choose()) #EG03-08BRANDS
brands_samp <- sample_n(brands,10)

#3.23

wshop<-read.csv(file.choose()) #EX03-23WSHOP
#creating two different groups
wshop_jun<-wshop[wshop$Associate=="Junior",]
wshop_sen<-wshop[wshop$Associate=="Senior",]

nrow(wshop)
wshop$rand<-runif(40,0,1)
wshop<-wshop[order(wshop$rand),] #Pick four juniors, two seniors

wshop_jun$rand<-runif(nrow(wshop_jun),0,1)
wshop_jun<-wshop_jun[order(wshop_jun$rand),] #Pick four juniors

wshop_sen$rand<-runif(nrow(wshop_sen),0,1)
wshop_sen<-wshop_sen[order(wshop_sen$rand),] #Pick two seniors

#3.31

resid<-read.csv(file.choose()) #EX03-31RESID
nrow(resid)
resid$rand<-runif(33,0,1)
resid<-resid[order(resid$rand),] #pick six apartments

#3.34 In using Table B repeatedly to choose samples, you should not always begin
#at the same place, such as line 101. Why not?

print('You do not want to start on the same place to avoid any repetitive numbers.')


#3.48

ccare<-read.csv(file.choose()) #EX03-48CCARE Company A & B brochures
nrow(ccare)
c(59636, 88804, 04634, 71197, 19352, 73089, 84898, 45785) #Table B line 112
c(59, 63, 68, 88, 04, 04, 63, 47, 11, 97, 19, 35, 27, 30, 89, 84, 89, 84, 57, 85) #5 observations
ccare$rand<-runif(40,0,1)
ccare<-ccare[order(ccare$rand),]
tgroup<-ccare[1:20,]
