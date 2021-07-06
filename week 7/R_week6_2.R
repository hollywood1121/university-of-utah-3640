#8.21
0.16*2006
0.16+qnorm(0.975,0,1)*sqrt(0.16*(1-0.16)/2006)
0.16-qnorm(0.975,0,1)*sqrt(0.16*(1-0.16)/2006)
#Because the numbers are self-reported, those who responded could be more or less likely to discuss their soft-drink
#consumption than those who didn't respond

#8.69
ctree<-read.csv(file.choose())
ctree$prop<-ctree$X.natural./ctree$n
pooled<-(ctree[1,3]+ctree[2,3])/(ctree[1,2]+ctree[2,2])
z<-(ctree[1,4]-ctree[2,4])/(pooled*sqrt(1/ctree[1,2]+1/ctree[2,2]))
1-pnorm(z,0,1) # multiply with 2 for two sided test, P > alpha, fail to reject the H0:p1=p2

zstar<-qnorm(.95,0,1)
ctree[1,4]-ctree[2,4]+zstar*sqrt((ctree[1,4]*(1-ctree[1,4])/ctree[1,2])+(ctree[2,4]*(1-ctree[2,4])/ctree[2,2]))
ctree[1,4]-ctree[2,4]-zstar*sqrt((ctree[1,4]*(1-ctree[1,4])/ctree[1,2])+(ctree[2,4]*(1-ctree[2,4])/ctree[2,2]))

#8.70
sumemp<-read.csv(file.choose())
empm<-sumemp[1,2]/sumemp[3,2]
empw<-sumemp[1,3]/sumemp[3,3]
pooled<-(sumemp[1,2]+sumemp[1,3])/(sumemp[3,2]+sumemp[3,3])
z<-(empm-empw)/(pooled*sqrt(1/sumemp[3,2]+1/sumemp[3,3]))
1-pnorm(z,0,1) # multiply with 2 for two sided test, P > alpha, fail to reject the H0:p1=p2

zstar<-qnorm(.975,0,1)
empm-empw+zstar*sqrt(empm*(1-empm)/sumemp[3,2]+empw*(1-empw)/sumemp[3,3])
empm-empw-zstar*sqrt(empm*(1-empm)/sumemp[3,2]+empw*(1-empw)/sumemp[3,3])
#Considering the data come from a university, the 1.4% to 8.2% difference likely accounts for hundreds of
#jobs, which would be practically important.