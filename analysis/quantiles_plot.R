
library(plotrix)
quantiles<-read.csv("quantiles.csv",stringsAsFactors=FALSE,header=T)

#quantiles<-quantiles[order(-quantiles$Quantile),]

plot(quantiles$Quantile,quantiles$SVM,type="l",col="green",xlim=rev(range(quantiles$Quantile)),
     ylim=c(0.45,0.9),xlab="model quantile by performance",ylab="RMSE",main="Performance of Models over Parameter Search Space")
grid(col="lightgrey")
lines(quantiles$Quantile,quantiles$NN,col="blue")
lines(quantiles$Quantile,quantiles$RF,col="red")

legend(0.2,0.9,legend=c("SVM","NN","RF"),pch=c(19,19),col=c("green","blue","red"))