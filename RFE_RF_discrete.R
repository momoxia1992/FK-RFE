RFE_RF_discrete<-function(x,y,ntree=200)
{
    Datam <- data.frame(y,x)
    x <- as.matrix(Datam[,-1])
    y <- as.matrix(Datam[,1])
    y=as.factor(y)#####使得randomForest成为classification关键！！！！
    remaining_features <- colnames(x)
    Variables <- 0
    oob <- Inf
    Top_10 <- NULL
    selected_predictors <- NULL
    number_of_variables <- NULL
    seqoob<-matrix(NA,nrow = 1,ncol=dim(x)[2])
    while(length(remaining_features) >= 2) {
      numremaining=length(remaining_features)
      Rf_reg <- randomForest(x[,remaining_features], y, ntree=ntree, importance=T )
      seqoob=Rf_reg$err.rate
      oobi=mean(seqoob[,1])
      
      w<-importance(Rf_reg,type=1)
      w<-w[order(w[,1]),,drop = F]
      if(oobi<=oob)
      {
        oob<-oobi
        Top_10<-tail(w,10)
        selected_predictors<-rownames(w)
        number_of_variables = nrow(w)
      }
      
      d<-rownames(w)
      w<-cbind(d,w)
      elim_feature<-w[1,1] # eliminating feature with lowest weight
      remaining_features<-remaining_features[remaining_features!=elim_feature]
    }
    od=selected_predictors
    c1=c(0)
    for (j in 1:length(od)) {
      for (i in 1:length(colnames(x))) {
        if(od[j]==colnames(x)[i]) c1[j]=i
      }
    }
    od1=c1[order(c1)]
    
    mylist<-list(number_of_variables=number_of_variables,selected_variables=od1,selected_variables2=selected_predictors,oob=oob)
  return ( mylist )  
}
