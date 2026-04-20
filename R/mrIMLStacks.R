


mrIMLbuild_models <- function(mrIMLObject){
  
  O <- mrIMLObject
 
####-----------------------Building from model details in object ---------------  
  
  #%%% The code below is horrible but will work for the demo.
  if (!O$Methodology$Stacking$StacksOnly){
    for (j in 1:length(O$Methodology$Models$modelNames)){
      eval(str2expression(paste0(
        "O$Models$",modelNames[[j]]
        ,"<-parsnip::",O$Methodology$Models$ModelFunction[[j]]
        ,"(engine = '",O$Methodology$Models$ModelEngines[j]
        ,"', mode = '",O$Methodology$Models$mode[j] 
        ,"')")))
    }
  }
  
  if(O$Methodology$Stacking$stackMethod == "control_stack_grid"){
    ctrl_grid <- stacks::control_stack_grid()
  } else if (O$Methodology$Stacking$stackMethod == "control_stack_resamples"){
    ctrl_grid <- control_stack_resamples()
  } else if (O$Methodology$Stacking$stackMethod == "control_stack_bayes"){
    ctrl_grid <- control_stack_bayes()
  } 
  #else {
  #  print("No appropriate control found")
  #  stop()
  #}
    
  
  if(max(O$Methodology$Models$stacking)==1){
    
    
    
    
  
  }
  
  modelList <- O$Methodology$Models$modelNames
  
  n_response <- length(O$Data$Y)
  n_models <- length(modelList)
  yhats <- future.apply::future_lapply(
    X = seq(1,n_models),
    FUN = function(i){
      mrIML_internal_fit_stacks(i, O)
    },
    future.seed = TRUE
  )  
  return(O)
}




mrIML_internal_fit_stacks <- function(i,O){
  ## Breaking down the object for readability:
  .X <- O$Data$X
  .X1 <- O$Data$X1
  .Y <- O$Data$Y
  
  
  
  
  
  
  
  
  
  
}
  
  
  
  