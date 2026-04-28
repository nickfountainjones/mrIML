#library(dplyr)
#library

new_mrIMLObject <-
  function(Methodology = NULL
           , Data = NULL
           , Premodel = NULL
           , Models = NULL
           , Fits = NULL
           , Bootstrap = NULL
           , Statistics = NULL) {
    # stopifnot(is.list(Methodology))
    # stopifnot(is.list(Data))
    # stopifnot(is.list(Models))
    # stopifnot(is.list(Fits))
    # stopifnot(is.list(Statistics))
    # 
    
    
    
    args <- list(
      Methodology   = enquo(Methodology)
      , Data  = enquo(Data)
      , Premodel  = enquo(Premodel)
      , Models = enquo(Models)
      , Fits = enquo(Fits)
      , Bootstrap = enquo(Bootstrap)
      , Statistics = enquo(Statistics)
    )
    
  }


includeModel.mr_IML_Object <-
  function(object
           , ModelList){
      for(i in ModelList){
        print(i)
        if (i == "RandomForest"){
          print("Building")
          model_rf <- parsnip::rand_forest(
            trees=100,
            mode = "classification",
            mtry=tune(),
            min_n=tune()
          ) %>%
            set_engine("randomForest")
          object$Models$model_rf=model_rf
        }
      }    
    return(object)
    
    
  }


validate_mr_IML_Object <- function(x) {
  values <- unclass(x)
  
}



temp_mrIML <- new_mrIMLObject()

modelListing = c("RandomForest","SupportVectorMachine")
  
includeModel.mr_IML_Object(temp_mrIML,modelListing)
