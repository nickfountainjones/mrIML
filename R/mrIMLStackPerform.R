

mrIMLStackPerform_classification <- function(Ob){
  response <- Ob$Methodology$Data$YHeadings
  #models <- attributes(Ob$Methodology$Models)$names
  models <- attributes(Ob$Models[[response[[1]]]])$names
  OP_Struct <- list()
  SummaryTable <- list()
 # EmptyDataFrame <- data.frame()
  ### Building Summary Statistics Section

  for (j in models){
    Ob$SummaryStatistics[[j]] <-  data.frame() 
  }
  
  tempTab2 <- list()
  tempTab3 <- list()
  
  for (i in 1:length(response)){
    
    #OP_Struct[[response[i]]] <- list()
    tempTab <- list()
    tempTab2[[response[[i]]]] <- list()
    for (j in 1:length(models)){
## Check for is there a model here and is it valid!

 #     predictData <- S$System[[response[i]]][[models[j]]]$final_fit$.predictions[[1]]
      predictData <- S$System[[response[i]]][[models[j]]]$last_model_fit$.predictions[[1]]
      
      
      sensTemp <- yardstick::sens(data = predictData,
                                  truth = paste0(response[[i]]),
                                  estimate = .pred_class)
      aucTemp <- yardstick::roc_auc(data = predictData,
                                    paste0(response[[i]]),
                                    .pred_0)
      
      brierTemp <- yardstick::brier_class(data = predictData,
                                          truth = paste0(response[[i]]),
                                          .pred_0)
      
      
      accTemp <- yardstick::accuracy(data = predictData,
                                     truth = paste0(response[[i]]),
                                     .pred_class)
      
      
      mccTemp <- yardstick::mcc(data = predictData,
                                     truth = paste0(response[[i]]),
                                     .pred_class)
      if(is.na(mccTemp$.estimate)){
        mccTemp$.estimate <- 0
      }
      
      
      ppvTemp <- yardstick::ppv(data = predictData,
                                truth = paste0(response[[i]]),
                                .pred_class)
      
      npvTemp <- yardstick::npv(data = predictData,
                                truth = paste0(response[[i]]),
                                .pred_class)
      if(is.na(npvTemp$.estimate)){
        npvTemp$.estimate <- 0
      }      
      
      specifTemp <- yardstick::specificity(data = predictData,
                                           truth = paste0(response[[i]]),
                                           .pred_class)
      
      
      prevTemp <- data.frame(.metric = "prevalence", .estimator = "binary",
                             .estimate = sum(as.numeric(predictData[[response[[i]]]])-1) 
                             / length(predictData[[response[[i]]]]))
      
      SummaryTable <- data.frame() %>%
        rbind(accTemp) %>%
        rbind(aucTemp) %>%
        rbind(brierTemp) %>%
        rbind(sensTemp) %>% 
        rbind(mccTemp) %>%
        rbind(ppvTemp) %>%
        rbind(npvTemp) %>%
        rbind(specifTemp) %>%
        rbind(prevTemp)
      
      tempTab[[models[j]]] <- SummaryTable$.estimate
      
      #tempTab2[[response[i]]][[models[j]]] <- SummaryTable$.estimate
      tempTab3[[models[j]]][[response[i]]] <- SummaryTable$.estimate

    }
    Ob$SummaryStatistics[[response[i]]] <- data.frame(tempTab)
    rownames(Ob$SummaryStatistics[[response[i]]]) <- SummaryTable$.metric

  }
  
  
  ### Stacking area
  

  
  
  
  #for different ys
  
  #print(attributes(Ob$SummaryStatistics)$names)
  
  for (j in models){
    # print(j)
    Ob$SummaryStatistics[[j]] <- data.frame(tempTab3[[j]])
    rownames(Ob$SummaryStatistics[[j]]) <- SummaryTable$.metric
    } 
  
  
  return(Ob)
}





