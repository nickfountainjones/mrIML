mrIML_StackLight <- function(Ob){
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
  
  models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")
  Ob$PD <- list()
  Ob$PD$FL <- list()
  Ob$PD$LP <- list()
  flPred_fun <- function(m, dat){
      pred <- stats::predict(m,
                             new_data = dat,
                             type = "prob"
      )
      return(dplyr::pull(pred,.data$.pred_1))
    }  
  for (k in models){
  # k <- "ModelStack"
    Ob$PD$FL[[k]] <- list()
    Ob$PD$LP[[k]] <- list()
    
    for (i in Ob$Methodology$Data$YHeadings){
      # i <- "Hzosteropis"
      
      if (k == "ModelStack"){
        flModelS <- Ob$Fits[[i]][[k]]
      } else {
        flModelS <- Ob$Fits[[i]][[k]]$last_model_fit$.workflow[[1]]
        
      }
  #    flData <- cbind(mrIML_lm$Data$Y, mrIML_lm$Data$X)
      flData <- Ob$Data
      flY <- i
      flLabel <- i
      flOutput <- flashlight::flashlight(model = flModelS, 
                                         label = flLabel,
                                         data = flData,
                                         y = flY,
                                         predict_function = flPred_fun)
      
      Ob$PD$FL[[i]][[k]] <- flOutput 
      
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












