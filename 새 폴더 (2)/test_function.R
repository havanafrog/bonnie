#########################
####    function     ####
#########################
n<-400
index<- 1:n

test.ft <- function(data,index){
  cat("start","\n")
  result0<-Mclust(data=data[index,],G=2,modelNames = "VVI")
  result1<-clustMD(X=data[index,],G=2,CnsIndx = 3,OrdIndx = 3,model="VVI",MaxIter = 1000)
  
  n_data <- matrix(0,ncol=ncol(data),nrow=nrow(data))
  n_data[,1]<-cut(data[,1],breaks = seq(from = max(data[,1])+1,to=min(data[,1])-1,length.out = 3),labels=c(1:2),right=F,include.lowest = T)
  
  mm2<-(max(data[,2])-min(data[,2]))
  
  cutp2<- c(-Inf, 
            (1*mm2/3+min(data[,2])), 
            (2*mm2/3+min(data[,2])),
            Inf)
  
  n_data[,2]<-cut(data[,2],breaks = cutp2,labels=c(1:3),right=T,include.lowest = T)
  
  mm3<-(max(data[,3]-min(data[,3])))
  
  cutp3<- c(-Inf, 
            (1*mm3/7+min(data[,3])), 
            (3*mm3/7+min(data[,3])),
            Inf)
  
  n_data[,3]<-cut(data[,3],breaks = cutp3,labels=c(1:3),right=T,include.lowest = T)
  result2<-Mclust(data=n_data[index,],G=2)
  
  result3<-clustMD(X=n_data[index,],G=2,CnsIndx = 0,OrdIndx = 3,MaxIter = 500,model="VII",startCL = "kmeans")
  
  f_data<-data.frame(n_data)
  kk<-homals(f_data,level=rep("ordinal",3),itermax=1000,ndim=1,rank=1)
  mew<-kk$low.rank
  
  resultk<-NULL
  
  resultk<-matrix(0,dim(f_data)[1],dim(f_data)[2])
  
  for (i in 1:3){
    test1 <- data.frame(cbind(rownames(kk$low.rank[[i]]),kk$low.rank[[i]]))
    test2 <- data.frame(f_data[i])
    colnames(test2) <- "V1"
    resultk[,i]<-as.numeric(as.matrix(test1$X1[match(test2$V1,test1$V1)]))
  }
  
  result4<-Mclust(data=resultk[index,],G=2,modelNames = "EII")
  
  if(result0$classification[1]==1){
    table0<-table(result0$classification,cate[,1])/n
  }else{
    table0<-table(result0$classification,rcate[,1])/n
  }
  
  if(result1$cl[1]==1){
    table1<-table(result1$cl,cate[,1])/n
  }else{
    table1<-table(result1$cl,rcate[,1])/n
  }
  
  if(result2$classification[1]==1){
    table2<-table(result2$classification,cate[,1])/n
  }else{
    table2<-table(result2$classification,rcate[,1])/n
  }
  
 
  
  if(result4$classification[1]==1){
    table4<-table(result4$classification,cate[,1])/n
  }else{
    table4<-table(result4$classification,rcate[,1])/n
  }
  
  if(is.na(result3$cl)[1]==TRUE){
    fi<-cbind(table0[1,1]+table0[2,2],
              table1[1,1]+table1[2,2],
              table2[1,1]+table2[2,2],
              NA,
              table4[1,1]+table4[2,2])
    
  }else{
    if(result3$cl[1]==1){
      table3<-table(result3$cl,cate[,1])/n
      
      fi<-cbind(table0[1,1]+table0[2,2],
                table1[1,1]+table1[2,2],
                table2[1,1]+table2[2,2],
                table3[1,1]+table3[2,2],
                table4[1,1]+table4[2,2])
      
    }else{
      table3<-table(result3$cl,rcate[,1])/n
      
      fi<-cbind(table0[1,1]+table0[2,2],
                table1[1,1]+table1[2,2],
                table2[1,1]+table2[2,2],
                table3[1,1]+table3[2,2],
                table4[1,1]+table4[2,2])
      }
  
  
  #list(table=fi,
  #  G=G,
  #  model=model,
  #  n=n)

  return(fi)
  }
  }
R<-10
z<-array(NA,c(R,5))
pb<-progress_estimated(R)
for (i in 1:R){
  z[i,]<-test.ft(data=data,index=sample(n,n,replace=T))
  cat(i,"done","\n")
  pb$tick()$print()
}

apply(z,2,mean)
boot::boot(data=data,R=10,statistic = test.ft)
