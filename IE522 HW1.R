# Title     : TODO
# Objective : TODO
# Created by: mavy731
# Created on: 8/28/20

#FALL20
#IE522 HW1

setwd("/home/mavy731/Documents/IE522/HW1")

TSLA<-read.csv('TSLA.csv',header=T)
lagdf<-function(x,k){
  c(rep(NA,k),x)[1:length(x)]
}
Price<-TSLA$Adj.Close
lgr=log(Price/lagdf(Price,1))

lgmean<-mean(lgr,na.rm=TRUE)
print("Mean")
round(lgmean,3)
lgstd<-sd(lgr,na.rm=TRUE)
print("Std")
round(lgstd,2)

library(moments)
print("SKewness")
skewness(lgr,na.rm=TRUE)
print("Kurtosis")
kurtosis(lgr,na.rm=TRUE)

print("Median")
median(lgr,na.rm=TRUE)

df<-data.frame(lgr)
lgr2<-lagdf(df$lgr,2)
df$lgr2<-lgr2
print("Correlation")
cor(df$lgr,df$lgr2,use="complete.obs")

print("3rd quantile")
quantile(df$lgr,probs=0.75,na.rm=TRUE,type=7)

