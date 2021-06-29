mu1<-c(0,0,0)
mu2<-c(1,-1,2)

sigma <- function(x){
  matrix(c(1,x,x,x,1,x,x,x,1),3)
}

n<-400
r1 <- rmvnorm(n*0.3,mean=mu1,sigma=sigma(0))
r2 <- rmvnorm(n*0.7,mean=mu2,sigma=sigma(0.3))
data <- rbind(r1,r2)
cate <- data.frame(c(rep("1",times=n*0.3),rep("2",times=n*0.7)))
rcate <- data.frame(c(rep("2",times=n*0.3),rep("1",times=n*0.7)))
N_data <- cbind(data,cate)

test.ft2 <- function(data,index){
  cat("start","\n")
  #result0<-Mclust(data=data[index,],G=2,modelNames = "VVI")
  #result1<-clustMD(X=data[index,],G=2,CnsIndx = 3,OrdIndx = 3,model="VVI",MaxIter = 1000)
  
  n_data <- matrix(0,ncol=ncol(data),nrow=nrow(data))
  n_data[,1]<-cut(data[,1],breaks = seq(from = max(data[,1])+1,to=min(data[,1])-1,length.out = 3),labels=c(1:2),right=F,include.lowest = T)
  
  mm2<-(max(data[,2])-min(data[,2]))
  
  cutp2<- c(-Inf, 
            (1*mm2/3+min(data[,2])), 
            (2*mm2/3+min(data[,2])),
            Inf)
  
  n_data[,2]<-cut(data[,2],breaks = cutp2,labels=c(1:3),include.lowest = F)
  
  mm3<-(max(data[,3]-min(data[,3])))
  
  cutp3<- c(-Inf, 
            (1*mm3/7+min(data[,3])), 
            (3*mm3/7+min(data[,3])),
            Inf)
  
  n_data[,3]<-cut(data[,3],breaks = cutp3,labels=c(1:3),include.lowest = F)
  #result2<-Mclust(data=n_data[index,],G=2)
  
  result3<-clustMD(X=n_data[index,],G=2,CnsIndx = 0,OrdIndx = 3,MaxIter = 1000,model="VII",startCL = "kmeans")
  
  #f_data<-data.frame(n_data)
  #kk<-homals(f_data,level=rep("ordinal",3),itermax=1000,ndim=1,rank=1)
  #mew<-kk$low.rank
  
  #resultk<-NULL
  
  #resultk<-matrix(0,dim(f_data)[1],dim(f_data)[2])
  
  #for (i in 1:3){
  #  test1 <- data.frame(cbind(rownames(kk$low.rank[[i]]),kk$low.rank[[i]]))
  #  test2 <- data.frame(f_data[i])
  #  colnames(test2) <- "V1"
  #  resultk[,i]<-as.numeric(as.matrix(test1$X1[match(test2$V1,test1$V1)]))
  #}
  
  #result4<-Mclust(data=resultk[index,],G=2,modelNames = "EII")
  
  #if(result3$cl[1]==1){
   # table3<-table(result3$cl,cate[,1])/n
  #}else if(result3$cl[1]==1){
  #  table3<-table(result3$cl,rcate[,1])/n
  #}
  
  fi<-c(table(result3$cl[1],exclude = NULL),result3$likelihood.store[1],result3$Sigma[1],result3$Sigma[10])
  
  return(fi)
}

R<-40
z<-array(NA,c(R,4))
for (i in 1:R){
  z[i,]<-test.ft2(data=data,index=sample(n,n,replace=T))
  cat(i,"done","\n")
}
