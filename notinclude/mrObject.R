library(parsnip)
library(tibble)
library(tidyverse)

new_mrIMLObj <- function(
    Y = tibble::tibble(),
    X = tibble::tibble(),
    X1 = tibble::tibble()
  ){
  
  
  #mode <- Model$mode
  
  # Coerce data to tibbles
  X <- tibble::as_tibble(X)
  Y <- tibble::as_tibble(Y)
  X1 <- tibble::as_tibble(X1)
  #Data <- list(X=X,Y=Y,X1=X1)
  

  
  # for (i in 1:length(models)){
  #   Models$models[[i]]<- parsnip::rand_forest()
  #   
  #   
  # }
  
  #Models <- models
  Data <- list()
  SummaryStatistics <- list()
  SetupSummary <- list()
  Models <- list()
  
  #mrIMLObj <- tibble::tibble(Data, Models,SummaryStatistics,SetupSummary)
  mrIMLObj <- list()
  mrIMLObj$Data <- Data
  mrIMLObj$Data$X <- X
  mrIMLObj$Data$Y <- Y
  mrIMLObj$Data$X1 <- X1
  
  mrIMLObj$Models <- Models
  mrIMLObj$SummaryStatistics <- SummaryStatistics
  mrIMLObj$SetupSummary <- SetupSummary
  
  
  return(mrIMLObj)
}
  

mrAddModels <- function(mrObj,ModelList)
  {
  for(i in ModelList){
    print(i)
    if (i == "RandomForest"){
      model_rf <- parsnip::rand_forest(
        trees=100,
        mode = "classification",
        mtry=tune(),
        min_n=tune()
        ) %>%
        set_engine("randomForest")
        mrObj$Models$model_rf=model_rf
    }
  }
  
  
  return(mrObj)
  
}






#tempMrIML= new_mrIMLObj(X=X,Y=X,X1=Y)
tempMrIML <- new_mrIMLObj(
#  models = "rand_forest",
  X = X,
  Y = Y,
  X1 = Y
)



tempMrIML2 <- new_mrIMLObj(X=X,Y=Y,X1=Y)
ModelList = c("RandomForest","Linear") 

#tempMrIML2 <- mrAddModels(tempMrIML2, ModelList)



tempMrIML2$Models$mrIML_rf <- mrIMLpredicts(
  X = tempMrIML2$Data$X,
  Y = tempMrIML2$Data$Y,
  X1 = tempMrIML2$Data$X1,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)





