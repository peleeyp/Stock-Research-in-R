#10/25/2013

##Task 1 Output: Find the stocks at their 90 days lowest and plot them into one pdf file.
Resull3000 <- read.csv("E:/Google Drive/Projects/Programmer/stock reasearch/Resull3000/Russell3000.csv")
Names <- read.csv("E:/Google Drive/Projects/Programmer/stock reasearch/Resull3000/names.csv")
Benchmark <- read.csv("E:/Google Drive/Projects/Programmer/stock reasearch/Resull3000/Benchmark.csv")



##---------------Formatting--------------------
del=c((1:(length(Resull3000)/3))*3)
Resull3000 <- Resull3000[-1,-del]

##Dealing with the Benchmark
Benchmark<-tail(Benchmark,n=91)
Benchmark<-Benchmark[-91,]

#For the error in first row
Resull3000 <- Resull3000[-1,]


stocklist = list()
for(i in 1:(length(Resull3000)/2)){  #loop of stocks
  stocklist[[i]]<-data.frame(Resull3000[,2*i-1],Resull3000[,2*i])
  stocklist[[i]][,2]<-as.numeric(as.character(stocklist[[i]][,2]))
  stocklist[[i]][,1]<-as.Date(stocklist[[i]][,1],format="%Y/%m/%d")
  if(sum(is.na(stocklist[[i]][,2]))>0){
    stocklist[[i]]<-stocklist[[i]][-which(rowSums(is.na(stocklist[[i]]))>=1),]
  }
  stocklist[[i]]<-tail(stocklist[[i]],n=91)
  names(stocklist[[i]])<-c("Date",paste(Names[i,1]))
}



##---------------Initialing--------------------
ismin <- data.frame(cbind(rep(1,times=length(Resull3000)/2)))

colnames(ismin)<-c("h90")
h=90


##---------------Calculating--------------------
#caculate the ismin
for(j in 1:length(stocklist)){  #loop of stocks
  if(dim(stocklist[[j]])[1]>=91){ #Exclude some data that is shorter than 90
    for(i in (dim(stocklist[[j]])[1]-1):(dim(stocklist[[j]])[1]-h)){  #loop of days, =90 for here  
      if (stocklist[[j]][i,2]*1.01<=tail(stocklist[[j]][,2],n=1)){
        ismin[j,1]=0;
        break;
      }
    }
  }else{
    ismin[j,1]=2; #For error data like missing or shorter than 90, return 2 
  }
}


#Calculating the Daily Return of the result, get the result and variance data.frame
result = list()
id=1
variance=data.frame()

for(j in 1:length(stocklist)){  #loop of stocks
  if(ismin[j,]==1){
    result[[id]] <- stocklist[[j]]
    for(i in 1:h){  #loop of days, =90 for here  
      result[[id]][i,3] <- (stocklist[[j]][i+1,2]-stocklist[[j]][i,2])/stocklist[[j]][i,2]
    }
    variance[id,1] <- j
    variance[id,2] <- var(head(result[[id]][,3],n=h))
    variance[id,3] <- cor(head(result[[id]][,3],n=h),Benchmark[,2])
    id=id+1
  }
}

#Sort the variance from high var to low var, then use it to sort result
variance <- variance[order(variance[,2], decreasing = TRUE),]


setwd(paste("c://", sep =""))
pdf(file=paste("stock-plots", ".pdf", sep =""), bg="white")
par(mfrow=c(3,3))
for(j in 1:dim(variance)[1]){  #loop of stocks in result
  plot(stocklist[[variance[j,1]]],type="l")
  title(main=paste(Names[variance[j,1],1]))
}
dev.off()

#Result table
resulttable<-data.frame()
for(j in 1:dim(variance)[1]){  #loop of stocks
  resulttable[1,1]<-"Ticker"
  resulttable[j+1,1]<-paste(Names[variance[j,1],1])
  resulttable[1,2]<-"Names"
  resulttable[j+1,2]<-paste(Names[variance[j,1],2])
  resulttable[1,3]<-"Variance"
  resulttable[j+1,3]<-variance[j,2]
  resulttable[1,4]<-"Correlation coefficient"
  resulttable[j+1,4]<-variance[j,3]
  #write(paste("Ticker: ",Names[variance[j,1],1], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    #write(paste(": ",Names[variance[j,1],2], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    #write(paste(": ",variance[j,2], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    #write(paste("Correlation coefficient: ",variance[j,3], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    #write("", file = paste("results",".txt", sep =""),append=TRUE)
}
write.csv(resulttable, file = "resulttable.csv")