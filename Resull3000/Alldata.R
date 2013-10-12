Russell1000 <- read.csv("E:/Dropbox/Python Programmer/Open_Jobs/Analytics/Stock Research/Russell 1000 - 2000-2013.csv",na.strings="#N/A")

Russell1000[,1]<-as.Date(Russell1000[,1],format="%m/%d/%Y")

Russell1000<-Russell1000[-1,]

ismin <- data.frame(cbind(rep(1,times=length(Russell1000))))

colnames(ismin)<-c("h90")
h=90


#Reduce the line which has too many NA value
#new<-Russell1000[-which(rowSums(is.na(Russell1000))>900),]

reducedata<-tail(Russell1000,n=180)


stocklist = list()
#Combine the date column to each stock column and store them in a list
for(j in 2:length(reducedata)){  #loop of stocks
  stocklist[[j-1]]<-data.frame(reducedata[,1],reducedata[,j])
}

#delete the NA row
for(i in 1:length(stocklist)){  #loop of stocks
  stocklist[[i]]<-stocklist[[i]][-which(rowSums(is.na(stocklist[[i]]))>=1),]
  #Pick the latest 90 days
  stocklist[[i]]<-tail(stocklist[[i]],n=91)  
  stocklist[[i]][,2]<-as.numeric(as.character(stocklist[[i]][,2]))
}


#caculate the ismin
for(j in 1:length(stocklist)){  #loop of stocks
 for(i in (dim(stocklist[[j]])[1]-h):(dim(stocklist[[j]])[1]-1)){  #loop of days, =90 for here
  if(dim(stocklist[[j]])[1]>=90){ #Exclude some data that is shorter than 90
   if (stocklist[[j]][i,2]<=stocklist[[j]][dim(stocklist[[j]])[1],2]){
    ismin[j,1]=0;
    break;
   }
  }else{
   ismin[j,1]=2; #For error data like missing or shorter than 90, return 2 
  }
 }
}


setwd(paste("c://", sep =""))

for(i in 1:length(stocklist)){  #loop of stocks
  if(ismin[i,]==1){
    write(paste("Stock Name: ",names(Russell1000)[i+1], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    write(paste("Today Price: ",tail(stocklist[[i]][,2],n=1), sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    write("", file = paste("results",".txt", sep =""),append=TRUE)
  }
}

###for testing the length
#for(j in 1:length(stocklist)){  #loop of stocks
#  for(i in (dim(stocklist[[j]])[1]-h):(dim(stocklist[[j]])[1]-1)){  #loop of days, =90 for here
#    if (dim(stocklist[[j]])[1]!=91){
#      b=j
#    }
#  }  
#}
###The result is No.830 stock has only 60 days