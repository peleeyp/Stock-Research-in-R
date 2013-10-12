Data <- read.csv("E:/Projects/Programmer/09.16.2013 stocks.csv")
#Calculate the Return for each stock in the portfolio
Return <- data.frame()
for(j in 2:31){
  for(i in 1:(dim(Data)[1]-1)){
    Return[i,j-1]<-as.data.frame((Data[i+1,j]-Data[i,j])/Data[i,j])
  }
}

#name them
colnames(Return)<-c("AA UN Equity","AXP UN Equity","BA UN Equity","BAC UN Equity","CAT UN Equity","CSCO UW Equity","CVX UN Equity","DD UN Equity","DIS UN Equity","GE UN Equity","HD UN Equity","HPQ UN Equity","IBM UN Equity","INTC UW Equity","JNJ UN Equity","JPM UN Equity","KO UN Equity","MCD UN Equity","MMM UN Equity","MRK UN Equity","MSFT UW Equity","PFE UN Equity","PG UN Equity","T UN Equity","TRV UN Equity","UNH UN Equity","UTX UN Equity","VZ UN Equity","WMT UN Equity","XOM UN Equity")

#Mean, Median, Min. and Max. of the daily return of each stock
summary(Return)


#Define the weights
#wt=c(rep(1/30,times=30))
wt=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#Calculate the weighted mean of return

#Calculate the un-weighted covariance
var<-cov(Return)
#Weight it
for(i in 1:30){
  for(j in 1:30){
    var[i,j]<-var[i,j]*wt[i]*wt[j]
  }
}
#Sum them to get the Risk
Risk<-sum(var)

#Calculate the means of daily return of each stock
meanReturn<-colMeans(Return)
#Calculate the weighted mean of the whole portfolio
ExpectedReturn = 0
for(i in 1:30){
  ExpectedReturn = ExpectedReturn + meanReturn[i]*wt[i]
}
names(ExpectedReturn)<-c("Expected Return")
jpeg('rplot.jpg')
plot(Risk,ExpectedReturn)
dev.off()