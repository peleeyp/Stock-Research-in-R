#9/19/2013

#Output: "ismin"
##Row 1 to Row 30 indicate 30 stocks, column "h7" to "h360" indicate different lengths of h days
##'0' means today's price is not the lowest price, '1' means today's price is the lowest price
##The last column "hmin" indicates the minimum length of h

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
  days<-0;
  for(i in (dim(Data[j])[1]-1):1){  #loop of day price from the most recent
    if (Data[i,j]>=Data[dim(Data)[1],j]){
      ismin[j-1,8]=days;
      days=days+1;
    }else{
      break;
    }
  }
}

pdf(file=paste("stock-plots", ".pdf", sep =""), bg="white")
#Nine per page
par(mfrow=c(3,3))
for(j in 2:31){  #loop of stocks
  setwd(paste("c://", sep =""))
  #dir.create(paste(names(Data)[j], sep ="")) 
  #This is for different folder
  #setwd(paste("c://",names(Data)[j], sep =""))
  for(p in 1:dim(h)[2]){  #loop of h7, h14,...,h360
    for(i in (dim(Data[j])[1]-h[1,p]):(dim(Data[j])[1]-1)){  #loop of days
      if (Data[i,j]<=Data[dim(Data)[1],j]){
        ismin[j-1,p]=0;
        break;
      }
    }
  }
  #This is for multiple files
  #pdf(file=paste(names(Data)[j],"-plots", ".pdf", sep =""), bg="white")
  write(paste("Stock Name: ",names(Data)[j], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
  for(p in 1:dim(h)[2]){  #loop of h7, h14,...,h360
    write(paste("When h=",h[1,p],", ismin=",ismin[j-1,p], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    #A demo code for putting results in different folders, but need to change more in this for loop
    #write("", file = paste(names(Data)[j],"-result",".txt", sep =""),append=TRUE)
    if(p==dim(h)[2]){
      write(paste("Min h=",ismin[j-1,8], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    }
  }
  write("", file = paste("results",".txt", sep =""),append=TRUE)
  #h[1,5]==90 for three months
  print(paste("When h=",h[1,p],", ismin=",ismin[j-1,p], sep =""), file = paste(names(Data)[j],"-plots", ".pdf", sep =""),append=TRUE)
  plot(Data[(dim(Data[j])[1]-(h[1,5]-1)):(dim(Data[j])[1]),1],Data[(dim(Data[j])[1]-(h[1,5]-1)):(dim(Data[j])[1]),j]
       ,xlab="Time", ylab="Price",type="l")
  title(main=paste(names(Data)[j], sep =""))
  #dev.off()
}
dev.off()


#For testing
#plot(Data[(dim(Data[2])[1]-(7-1)):(dim(Data[2])[1]),1],Data[(dim(Data[2])[1]-(7-1)):(dim(Data[2])[1]),4],xlab="Time", ylab="Price",type="l")

#Data[,1] <- format(as.Date(Data[,1]), "%d/%m/%Y")
