
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
mrIML_StackLight <- function(Ob){  
  models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")
  Ob$PD <- list()
  Ob$PD$FL <- list()
  Ob$PD$LP <- list()

  #' the following does not work for unknown reasons (likely piping?) so instead 
  #' a non piping version is used.
  # fl_setup <- mrIML_flashlight_setup(Ob$Methodology$Stacking$stackMode)  
  
  
  flpred_fun <- function(m, dat){
    pred <- stats::predict(m,
                           new_data = dat,
                           type = "prob"
    )
    return(dplyr::pull(pred,.data$.pred_1))
  }
  
  
  for (k in models){
    Ob$PD$FL[[k]] <- list()
    Ob$PD$LP[[k]] <- list()
    
    for (i in Ob$Methodology$Data$YHeadings){
      Ob$PD$FL[[i]][[k]] <- flashlight::flashlight(model = Ob$Fits[[i]][[k]]$last_model_fit$.workflow[[1]], 
                                         label = i,
                                         data = Ob$Data,
                                         y = i,
                                         predict_function = flpred_fun)
      for (j in Ob$Methodology$Data$XHeadings){
        Ob$PD$LP[[i]][[k]][[j]] <- light_profile(Ob$PD$FL[[i]][[k]], data = Ob$Data, v = j)
      }
    }
  }

  if(Ob$Methodology$Visualisation$partialDependency){
    mrIML_StackLight_Plot(Ob)
  }
  return(Ob)
  
}



#' Plotting tools.
#' @description 
#' mrIML_StackLight_Plot is a basic plot summary for all light profile information
#' not intended for final plots but should indicate how the process is going.


mrIML_StackLight_Plot<- function(Ob){
  plotList <- list()
  miscList <- list()
  miny <- 0.1
  maxy <- 0.2
  for(i in c(attributes(Ob$Methodology$Models)$name,"ModelStack")){
    for(j in Ob$Methodology$Data$YHeading){
      for(k in Ob$Methodology$Data$XHeadings){
        plotName <- paste(j, "-", i) 
        plotPoint <- paste0(j, "-", i) 
        miny <- min(miny,Ob$PD$LP[[j]][[i]][[k]]$data$value)
        maxy <- max(maxy,Ob$PD$LP[[j]][[i]][[k]]$data$value)
        plotList[[plotPoint]] <- plot(Ob$PD$LP[[j]][[i]][[k]],facet_scales = "fixed")+
          ggtitle(plotName)
      }
    }
  }
  
  colPlot <- patchwork::wrap_plots(plotList
                                   , axes = 'collect')& ylim(miny - 0.1, maxy + 0.1)
  print(colPlot)
}












