mrIML_StackLight <- function(Ob){
  #' Lots of checks of previous steps required here now...
  #' - Check for complete stacks object
  #' - Check that predictions have been run successfully.
  #' 
  #' 
  #' 
  #' 

  ### new temporary object in the form expected by mrFlashlight
  
  # if values are factors they need to be converted to doubles:
  
  Ob$PDCurves <- list()
  
  i <- "Hzosteropis"
  
  flModelS <- Ob$Fits$Hzosteropis$ModelStack
  flData <- cbind(mrIML_lm$Data$Y, mrIML_lm$Data$X)
  flY <- i
  flLabel <- i
  
  data <- Ob$Data
  
  
  
  flPred_fun <- function(m, dat){
    pred <- stats::predict(m,
                           new_data = dat,
                           type = "prob"
    )
    return(dplyr::pull(pred,.data$.pred_1))
  }
  
  
  flOutput <- flashlight::flashlight(model = flModel, 
                                     label = flLabel,
                                     data = flData,
                                     y = flY,
                                     predict_function = flPred_fun)
  
  Ob$PDCurves[[i]] <- flOutput 
  
  # for (i in attributes(tempData)$names) {
  #   if(is.factor(tempData[[i]])){
  #     tempData[[i]] <- as.numeric(levels(tempData[[i]]))[tempData[[i]]]
  #   }
  # }
  # 
  
  
  # 
  # dataX <- select(tempData, Ob$Methodology$Data$XHeadings)
  # tOb = list()
  
  return(Ob)
  
}