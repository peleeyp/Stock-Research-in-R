Data <- read.csv("E:/Projects/Programmer/09.16.2013 stocks.csv")
#Calculate the Yearly Return for each stock in the portfolio
YearReturn <- data.frame()
for(j in 2:31){
  for(i in 1:894){
    YearReturn[i,j-1]<-as.data.frame((Data[i+365,j]-Data[i,j])/Data[i,j])
  }
}
#name them
colnames(YearReturn)<-c("AA UN Equity","AXP UN Equity","BA UN Equity","BAC UN Equity","CAT UN Equity","CSCO UW Equity","CVX UN Equity","DD UN Equity","DIS UN Equity","GE UN Equity","HD UN Equity","HPQ UN Equity","IBM UN Equity","INTC UW Equity","JNJ UN Equity","JPM UN Equity","KO UN Equity","MCD UN Equity","MMM UN Equity","MRK UN Equity","MSFT UW Equity","PFE UN Equity","PG UN Equity","T UN Equity","TRV UN Equity","UNH UN Equity","UTX UN Equity","VZ UN Equity","WMT UN Equity","XOM UN Equity")

#Mean, Median, Min. and Max. of the daily return of each stock
summary(YearReturn)


#Define the weights
#wt=c(rep(1/30,times=30))
wt=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#Calculate the un-weighted covariance
yearVar<-cov(YearReturn)
#Weight it
for(i in 1:30){
  for(j in 1:30){
    var[i,j]<-var[i,j]*wt[i]*wt[j]
  }
}
#Sum them to get the Risk
yearRisk<-sum(yearVar)

#Calculate the means of daily return of each stock
meanyearReturn<-colMeans(YearReturn)
#Calculate the weighted mean of the whole portfolio
ExpectedyearReturn = 0
for(i in 1:30){
  ExpectedyearReturn = ExpectedyearReturn + meanyearReturn[i]*wt[i]
}
names(ExpectedyearReturn)<-c("Expected Return")