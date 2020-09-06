# Title     : TODO
# Objective : TODO
# Created by: mavy731
# Created on: 9/5/20

#Fall20
#IE522HW2

#install.packages('ISLR')
#install.packages('VGAM')
#install.packages('xts')
library(ISLR)
n=nrow(Smarket)
Smarket[c(1:3,(n-2):n),]

setwd("/home/mavy731/Documents/IE522/HW2")
dates=read.csv("ISLRSmarketDates.csv",header=TRUE)
sp=data.frame(dates,Smarket[,-1])
n=nrow(sp)
sp[c(1:3,(n-2):n),]

hist(sp$Today,breaks=50,col='azure4',prob=TRUE)

hist(sp$Today,breaks=50,col='azure4',prob=TRUE)
TdMean=mean(sp$Today, na.rm=TRUE)
TdStd=sd(sp$Today, na.rm=TRUE)
curve(dnorm(x,TdMean,TdStd),add=TRUE,col="chocolate3",lwd=4)

library(VGAM)
hist(sp$Today,breaks=50,col='azure4',prob=TRUE)
TdMean=mean(sp$Today, na.rm=TRUE)
TdStd=sd(sp$Today, na.rm=TRUE)
curve(dnorm(x,TdMean,TdStd),add=TRUE,col="chocolate3",lwd=3)
curve(dlaplace(x,TdMean,TdStd),add=TRUE,col='cyan',lwd=3)

hist(sp$Today,breaks=50,col='azure4',prob=TRUE)
lines(density(sp$Today,bw="nrd0",adjust=0.25,na.rm=TRUE),col="blueviolet",lwd=3)
lines(density(sp$Today,bw="nrd0",adjust=4,na.rm=TRUE),col="brown2",lwd=3)

set.seed(2)
boxplot(sp$Today,ylab='Percentage Return')

outliers=boxplot(sp$Today,plot=FALSE)$out
sp[sp$Today %in% min(outliers),]

 round(cor(sp[,2:8],use="complete.obs"),digits=2)

pairs(sp[,2:8])

 library(xts)
 library(zoo)
d<-as.Date(sp$Date,"%m/%d/%Y")
sp_ts <-xts(sp[2:8],order.by=d)

plot(sp_ts$Volume)

normaldata<-rnorm(1000,mean=TdMean,sd=TdStd)
par(mfrow=c(1,2))

qqnorm(sp$Today,ylab="Sample Quantiles of Today",xlim=c(-6,6))
qqline(sp$Today)
qqplot(rlaplace(1000),sp$Today,xlim=c(-6,6))
qqline(sp$Today)