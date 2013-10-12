Data <- read.csv("E:/Projects/Programmer/09.16.2013 stocks.csv")
#Calculate the Yearly Return for each stock in the portfolio
monthReturn <- data.frame()
for(j in 2:31){
  for(i in 1:1229){
    monthReturn[i,j-1]<-as.data.frame((Data[i+30,j]-Data[i,j])/Data[i,j])
  }
}
#name them
colnames(monthReturn)<-c("AA UN Equity","AXP UN Equity","BA UN Equity","BAC UN Equity","CAT UN Equity","CSCO UW Equity","CVX UN Equity","DD UN Equity","DIS UN Equity","GE UN Equity","HD UN Equity","HPQ UN Equity","IBM UN Equity","INTC UW Equity","JNJ UN Equity","JPM UN Equity","KO UN Equity","MCD UN Equity","MMM UN Equity","MRK UN Equity","MSFT UW Equity","PFE UN Equity","PG UN Equity","T UN Equity","TRV UN Equity","UNH UN Equity","UTX UN Equity","VZ UN Equity","WMT UN Equity","XOM UN Equity")

#Mean, Median, Min. and Max. of the daily return of each stock
summary(monthReturn)


#Define the weights
#wt=c(rep(1/30,times=30))
wt=c(1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

#Calculate the un-weighted covariance
monthVar<-cov(monthReturn)
#Weight it
for(i in 1:30){
  for(j in 1:30){
    var[i,j]<-var[i,j]*wt[i]*wt[j]
  }
}
#Sum them to get the Risk
monthRisk<-sum(monthVar)

#Calculate the means of daily return of each stock
meanmonthReturn<-colMeans(monthReturn)
#Calculate the weighted mean of the whole portfolio
ExpectedmonthReturn = 0
for(i in 1:30){
  ExpectedmonthReturn = ExpectedmonthReturn + meanmonthReturn[i]*wt[i]
}
names(ExpectedmonthReturn)<-c("Expected Return")