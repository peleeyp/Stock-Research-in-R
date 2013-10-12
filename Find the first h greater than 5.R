#9/19/2013

#Output: "ismin"
##The key is to find the first h>=5, if h<t, then we find the the date before today

Data <- read.csv("E:/Google Drive/Projects/Programmer/stock reasearch/09.16.2013 stocks.csv")

Data[,1]<-as.Date(Data[,1],format="%Y/%m/%d")
colnames(Data)<-c("Date","AA UN Equity","AXP UN Equity","BA UN Equity","BAC UN Equity","CAT UN Equity","CSCO UW Equity","CVX UN Equity","DD UN Equity","DIS UN Equity","GE UN Equity","HD UN Equity","HPQ UN Equity","IBM UN Equity","INTC UW Equity","JNJ UN Equity","JPM UN Equity","KO UN Equity","MCD UN Equity","MMM UN Equity","MRK UN Equity","MSFT UW Equity","PFE UN Equity","PG UN Equity","T UN Equity","TRV UN Equity","UNH UN Equity","UTX UN Equity","VZ UN Equity","WMT UN Equity","XOM UN Equity")

###A matrix showing that if the today's price is the lowest in h days
ismin <- data.frame(cbind(1,rep(1,times=30),1,1,1,1,1),0)

colnames(ismin)<-c("h7","h14","h30","h60","h90","h120","h360","minh")
h <- data.frame(cbind(7,14,30,60,90,120,360))
colnames(h)<-c("h7","h14","h30","h60","h90","h120","h360")


###Calculate the number of days which the price is higher than today



for(j in 2:31){  #loop of stocks
  for(count in 0:(dim(Data[j])[1]-5)){
  days<-0;
    for(i in (dim(Data[j])[1]-1-count):1){  #loop of day price from the most recent
      if(Data[i,j]>=Data[dim(Data[j])[1]-count,j]){
        ismin[j-1,8]=days+2;
        days=days+1;
        if(days==5){
          write(paste("Stock Name: ",names(Data)[j], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
          write(paste("The first h>=5 when the date is:",Data[dim(Data[j])[1]-count,1], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
          
        }
      }
      else{
        break;
      }
    }
    if(ismin[j-1,8]>=5){
      write(paste("h is:",ismin[j-1,8], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
      write("", file = paste("results",".txt", sep =""),append=TRUE)
      break;
    }
  }
}