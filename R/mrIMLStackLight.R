
  #' Lots of checks of previous steps required here now...
  #' - Check for complete stacks object
  #' - Check that predictions have been run successfully.
  #' @description 
  #' mrFlashlight can not be used in current version due to issues with the way
  #' stats::predict interacts with stacks when dealing with classification
  #' (likely sub models can not have automatic double to factor transformations)
  #' due to this a simplified predictive function is created and each of the  
  #' 
  #' @param Ob is an object previously created using mrBuildModels.
  #'        Control for plot in original parameters
  #'        
  #' @details 
  #' `mrIML_StackLight` 
  #'
  
  # 
mrIML_StackLight <- function(Ob, response = "single"){  
  models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")
  Ob$GMAM <- list()
  Ob$GMAM$FL_S <- list()
  Ob$GMAM$LP_S <- list()
  Ob$GMAM$FL_M <- list()
  Ob$GMAM$LP_M <- list()

  #' the following does not work for unknown reasons (likely piping?) so instead 
  #' a non piping version is used.
  # fl_setup <- mrIML_flashlight_setup(Ob$Methodology$Stacking$stackMode)  
  
  
  setup <- mrIML_StackLight_setup(Ob)
  
  
  for (i in Ob$Methodology$Data$YHeadings){
    for (k in models){
 #   Ob$GMAM$FL_S[[k]] <- list()
 #   Ob$GMAM$LP_S[[k]] <- list()
        Ob$GMAM$FL_S[[i]][[k]] <- flashlight::flashlight(model = Ob$Fits[[i]][[k]]$last_model_fit$.workflow[[1]], 
                                           label = i,
                                           data = Ob$Data,
                                           y = i,
                                           predict_function = setup$pred_fun,
                                           metrics = setup$metrics)
        # for (j in Ob$Methodology$Data$XHeadings){
          for (X1 in c(Ob$Methodology$Data$XHeadings,Ob$Methodology$Data$YHeadings[!Ob$Methodology$Data$YHeadings %in% i])){
            Ob$GMAM$LP_S[[i]][[k]][[X1]] <- light_profile(Ob$GMAM$FL_S[[i]][[k]], data = Ob$Data, v = X1)
          } #X1
          # Ob$GMAM$LP_S[[i]][[k]][[j]] <- light_profile(Ob$GMAM$FL_S[[i]][[k]], data = Ob$Data, v = j)
        # } #j 
      } #k
    } #i
  #   
  #   
  #   ## Multi
  flLists <- list()
  Ob$GMAM$FL_M <- list()
  profiles_lpS <- list()
  profiles_dfS <- list()
  profiles_plots <- list()
  
  for (j in Ob$Methodology$Data$XHeadings){
    flLists[[j]] <- list()
    Ob$GMAM$FL_M[[j]] <- list()
    for (k in models){
      flLists[[j]][[k]] <- list()
      for (i in Ob$Methodology$Data$YHeadings){
        flLists[[j]][[k]][[i]] <- Ob$GMAM$FL_S[[i]][[k]]
      } #i
      Ob$GMAM$FL_M[[j]][[k]] <- flashlight::multiflashlight(flLists[[j]][[k]]
                                                            , data = Ob$Data
                                                            ,predict_function = setup$pred_fun
                                                            , metrics = setup$metrics) #metrics next?
      Ob$GMAM$LP_M[[j]][[k]] <- flashlight::light_profile(Ob$GMAM$FL_M[[j]][[k]], data = Ob$Data, v = j, type = "ale")
      
      
      #This section is for covariance. Maybe add an option to avoid this?
      
      Ob$GMAM$Cov[[j]][[k]] <- Ob$GMAM$LP_M[[j]][[k]]$data %>%
      dplyr::rename(cov_grid = .data[[j]]) %>%
      dplyr::group_by(.data$label) %>%
      dplyr::mutate(
        sd = stats::sd(.data$value),
        cov_grad = .data$cov_grid +
          (0.5 * (dplyr::lead(.data$cov_grid) - .data$cov_grid)),
        cov_diff = abs(dplyr::lead(.data$value) - .data$value))
    } #k
  } #j

# if(Ob$Methodology$Visualisation$partialDependency){
#   mrIML_StackLight_Plot(Ob)
# }
  return(Ob)
  
  
  
  
  
  
  
}


mrIML_StackLight_setup <- function(Ob, predict_function = NULL){
  if (Ob$Methodology$Stacking$stackMode == "classification") {
      pred_fun <- function(m, dat) {
        pred <- stats::predict(m,
                               new_data = dat,
                               type = "prob"
        )
        return(dplyr::pull(pred,.data$.pred_1))
      }
      metrics <- list(
        logloss = MetricsWeighted::logLoss,
        `ROC AUC` = MetricsWeighted::AUC,
        `% Dev Red` = MetricsWeighted::r_squared_bernoulli
      )
  } else if (Ob$Methodology$Stacking$stackMode == "regression") {
    pred_fun <- function(m, dat) {
      pred <- stats::predict(m,
                             new_data = dat
      )
      return(dplyr::pull(pred,.data$.pred))
    }
    metrics <- list(
      rmse = MetricsWeighted::rmse,
      `R-squared` = MetricsWeighted::r_squared)
  }
  # Override pred_fun() if user supplied one
  if (!is.null(predict_function)) {
    pred_fun <- predict_function
  }    
  
  
  
  return(list(metrics = metrics, pred_fun = pred_fun))
  
}


#' Plotting tools.
#' @description 
#' mrIML_StackLight_Plot is a basic plot summary for all light profile information
#' not intended for final plots but should indicate how the process is going.
#' This is currently disabled as the functionality was moved to a generalised plotting
#' function (separate file)


# mrIML_StackLight_Plot<- function(Ob){
#   
# if (!is.null(Ob$GMAM$LP_S$ModelStack[1][[1]])) {
#   plotList <- list()
#   miscList <- list()
#   miny <- 0.1
#   maxy <- 0.2
#   for(i in c(attributes(Ob$Methodology$Models)$name,"ModelStack")){
#     for(j in Ob$Methodology$Data$YHeading){
#       for(k in Ob$Methodology$Data$XHeadings){
#         plotName <- paste(j, "-", i) 
#         plotPoint <- paste0(j, "-", i) 
#         print(Ob$GMAM$LP_S[[j]][[i]][[k]]$data$value)
#         miny <- min(miny,Ob$GMAM$LP_S[[j]][[i]][[k]]$data$value)
#         maxy <- max(maxy,Ob$GMAM$LP_S[[j]][[i]][[k]]$data$value)
#         plotList[[plotPoint]] <- plot(Ob$GMAM$LP_S[[j]][[i]][[k]])+
#           ggtitle(plotName)
#       }
#     }
#   }
#   
#   colPlot <- patchwork::wrap_plots(plotList
#                                    , axes = 'collect')& ylim(miny - 0.1, maxy + 0.1)
#   print(colPlot)
#   }
#   
# }












