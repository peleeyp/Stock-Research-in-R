Book1 <- read.csv("C:/Users/Administrator/Desktop/Book1.csv")

del=c((1:(length(Book1)/3))*3)
Book1 <- Book1[-1,-del]

stocklist = list()
for(i in 1:(length(Book1)/2)){  #loop of stocks
  stocklist[[i]]<-data.frame(Book1[,2*i-1],Book1[,2*i])
  stocklist[[i]][,2]<-as.numeric(as.character(stocklist[[i]][,2]))
  if(sum(is.na(stocklist[[i]][,2]))>0){
  stocklist[[i]]<-stocklist[[i]][-which(rowSums(is.na(stocklist[[i]]))>=1),]
  }
  stocklist[[i]]<-tail(stocklist[[i]],n=91)
}


ismin <- data.frame(cbind(rep(1,times=length(Book1)/2)))

colnames(ismin)<-c("h90")
h=90



#caculate the ismin
for(j in 1:length(stocklist)){  #loop of stocks
 if(dim(stocklist[[j]])[1]>=91){ #Exclude some data that is shorter than 90
  for(i in (dim(stocklist[[j]])[1]-1):(dim(stocklist[[j]])[1]-h)){  #loop of days, =90 for here  
   if (stocklist[[j]][i,2]*1.05<=tail(stocklist[[j]][,2],n=1)){

    ismin[j,1]=0;
    break;
     
   }
  }
 }else{
   ismin[j,1]=2; #For error data like missing or shorter than 90, return 2 
 }
}


setwd(paste("c://", sep =""))

for(i in 1:length(stocklist)){  #loop of stocks
  if(ismin[i,]==1){
    write(paste("Stock Name: ",names(Book1)[2*i-1], sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    write(paste("Today Price: ",tail(stocklist[[i]][,2],n=1), sep =""), file = paste("results",".txt", sep =""),append=TRUE)
    write("", file = paste("results",".txt", sep =""),append=TRUE)
  }
}


##For testing which stock is shorter than 90
#b=2
#for(j in 1:length(stocklist)){  #loop of stocks
#  for(i in (dim(stocklist[[j]])[1]-h):(dim(stocklist[[j]])[1]-1)){  #loop of days, =90 for here
#    if (dim(stocklist[[j]])[1]!=91){
#      b=j
#    }
#  }  
#}