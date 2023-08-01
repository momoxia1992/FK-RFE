SIS_RFE<-function(x=x.base, y=y.base, ntree=500,SISmethod="Kfilter",dn="NA",maxntype=30)
{
  n=dim(x)[1]
  p=dim(x)[2]
  n1=length(y)
  if(dn=='NA'){dn=n/log(n)}####Ä¬ÈÏdn=n/log(n)
  ####step 1 SIS####
  if(n1!=n){return("Different sample sizes for x and y")
    }else{
      y=as.vector(y)
      if(SISmethod=="Kfilter"){ 
        if (length(table(y)) <= maxntype){
          actset0=Kfilter_single(x, y, dn)}else{actset0=Kfilter(x,y,dn)}}
      if(SISmethod=="SIS") {actset0=SIS(x,y,dn)}
    }
  x2=x[,actset0]
  
  ###step 2 RFE####
  if (length(table(y)) <= maxntype){#######discrete
    stp2=RFE_RF_discrete(x=x2,y=y,ntree=ntree)
  }else{#####continuous
  stp2=RFE_RF(x=x2,y=y,ntree=ntree)}
  
  selected_predictors=actset0[stp2$selected_variables]
  mylist<-list(number_of_variables=length(selected_predictors),selected_variables=selected_predictors,filterselect=actset0)
  return(mylist)
}