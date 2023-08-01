RFE_RF<-function(x=x.base, y=y.base, ntree=200 )
{
  Datam <- data.frame(y,x)
  x <- as.matrix(Datam[,-1])
  y <- as.matrix(Datam[,1])
  remaining_features <- colnames(x)
  Variables <- 0
  mse <- Inf
  Top_10 <- NULL
  selected_predictors <- NULL
  number_of_variables <- NULL
  while(length(remaining_features) >= 3) {
    Rf_reg <- randomForest(x[,remaining_features], y, ntree=ntree, importance=T )
    # Prediction
    rf_predictions <- predict( Rf_reg, x[,remaining_features])
    rf_predictions <- as.data.frame(rf_predictions)
    rf_predictions <- cbind(rf_predictions,y)
    colnames(rf_predictions)<-c("prediction","test_y")
    rf_predictions <- as.data.frame(rf_predictions)
    rf_predictions <- rf_predictions%>%mutate(Test_error = (test_y-prediction)^2) #calculating relative error
    rf_predictions <- rf_predictions%>%mutate(Relative_test_error=abs((Test_error/test_y)))
    # Calculating mean Sqaured error
    Mean_Squared_error<-sum(rf_predictions[,3])/nrow(rf_predictions)
    # Variable Importance and eliminating features
    w<-importance(Rf_reg,type=1)
    w<-w[order(w[,1]),,drop = F]
    # Extracting top variables for minimum mse
    if( Mean_Squared_error<=mse )
    {
      mse<-Mean_Squared_error
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
  
  mylist<-list(number_of_variables=number_of_variables,selected_variables=od1,selected_variables2=selected_predictors,mse=mse)
  return ( mylist )
}