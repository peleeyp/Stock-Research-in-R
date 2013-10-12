#10/03/2013

#Find the lowest price in week, month and quarter
#Pick 10 stocks

Data <- read.csv("E:/Google Drive/Projects/Programmer/stock reasearch/09.16.2013 stocks.csv")

Data[,1]<-as.Date(Data[,1],format="%Y/%m/%d")
colnames(Data)<-c("Date","AA UN Equity","AXP UN Equity","BA UN Equity","BAC UN Equity","CAT UN Equity","CSCO UW Equity","CVX UN Equity","DD UN Equity","DIS UN Equity","GE UN Equity","HD UN Equity","HPQ UN Equity","IBM UN Equity","INTC UW Equity","JNJ UN Equity","JPM UN Equity","KO UN Equity","MCD UN Equity","MMM UN Equity","MRK UN Equity","MSFT UW Equity","PFE UN Equity","PG UN Equity","T UN Equity","TRV UN Equity","UNH UN Equity","UTX UN Equity","VZ UN Equity","WMT UN Equity","XOM UN Equity")


Data <- Data[,1:11]


###A matrix storing the lowest price
LowestPrice <- data.frame(cbind(0,rep(0,times=10),0))

colnames(LowestPrice)<-c("week","month","quarter")
h <- data.frame(cbind(7,30,90))
colnames(h)<-c("week","month","quarter")


###Calculate the number of days which the price is higher than today
for(j in 2:11){  #loop of stocks
 LowestPrice[j-1,]<-Data[dim(Data)[1],j]
 for(p in 1:3){
  for(i in (dim(Data[j])[1]-1):(dim(Data[j])[1]-h[,p])){  #loop of day price from the most recent
   if (Data[i,j]<=LowestPrice[j-1,p]){
    LowestPrice[j-1,p]=Data[i,j];
   }
  }
 }
}



setwd(paste("c://", sep =""))
for(j in 2:11){
write(paste("Stock Name: ",names(Data)[j], sep =""), file = paste("lowest",".txt", sep =""),append=TRUE)
write(paste("Lowest price in week: ",LowestPrice[j-1,1], sep =""), file = paste("lowest",".txt", sep =""),append=TRUE)
write(paste("Lowest price in month: ",LowestPrice[j-1,2], sep =""), file = paste("lowest",".txt", sep =""),append=TRUE)
write(paste("Lowest price in quarter: ",LowestPrice[j-1,3], sep =""), file = paste("lowest",".txt", sep =""),append=TRUE)
write(paste("", sep =""), file = paste("lowest",".txt", sep =""),append=TRUE)
}
