#' These are generic plots!
#' required libraries in parent functions:
#'  - ggplot2
#'  - dplyr


mrIMLStackPlot <- function(Ob, options = list()){
  if(length(options)==0){
    options$PDPPlot<-FALSE
    options$ALE <-FALSE
    options$CP <- FALSE
    options$DerivativePlots <- FALSE
    options$CovPlots <- FALSE
    options$StackOnly <- FALSE
    options$Export <- FALSE
  }
  plotOb <- list()
  exportOb <- list()

  #' This section is working on plots from the flashlight object.
  #' 
  #' Checks need to be done initially to check if the object has the flashlight objects
  #' already embedded. If not processing is skipped and an informative error should be
  #' returned.
  
  # Starting with the option of a Partial Derivative Plot section
  if(!is.null(options$StackOnly)&&options$StackOnly){
    models <- c("ModelStack")
  } else {
    models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")

  }
  
  
  if(!is.null(options$PDPPlot)&&options$PDPPlot){
    
    if (!is.null(Ob$GMAM$LP_S[[1]]$ModelStack)) {
      plotList <- list()
      miscList <- list()
      miny <- 0
      maxy <- 1
      for(i in Ob$Methodology$Data$YHeading){
        for(k in models){
          for(j in Ob$Methodology$Data$XHeadings){
            plotName <- paste(i, "-", k) 
            plotPoint <- paste0(i, "-", k) 
            miny <- min(miny,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
            maxy <- max(maxy,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
            pdpPlot <- ggplot2::ggplot(Ob$GMAM$LP_S[[i]][[k]][[j]]$data,
                                       ggplot2::aes(x = .data[[j]], y = .data$value))+
              ggplot2::geom_line()+
              geom_point() +
              ggtitle(plotName) +
              coord_cartesian(ylim = c(miny, maxy))
            plotOb$PDPPlots[[i]][[k]][[j]]<- ggpubr::as_ggplot(
              ggplot2::ggplotGrob(pdpPlot))
            exportOb$PDPPlots[[i]][[k]][[j]]<- ggplot2::ggplotGrob(pdpPlot)
            
          } #i
          
        } #j

      } #k
    }
      if (!is.null(Ob$GMAM$LP_S[[1]]$ModelStack)) {
        plotList <- list()
        miscList <- list()
        miny <- 0
        maxy <- 1
        for(i in Ob$Methodology$Data$YHeading){
          for(k in models){
            for(j in Ob$Methodology$Data$XHeadings){
              plotName <- paste(i, "-", k) 
              plotPoint <- paste0(i, "-", k) 
              alePlot <- ggplot2::ggplot(Ob$GMAM$ALE[[i]][[k]][[j]]$data,
                                         ggplot2::aes(x = .data[[j]], y = .data$value))+
                ggplot2::geom_line()+
                geom_point() +
                ggtitle(plotName) 
              plotOb$ALEPlots[[i]][[k]][[j]]<- ggpubr::as_ggplot(
                ggplot2::ggplotGrob(alePlot))
              exportOb$ALEPlots[[i]][[k]][[j]]<- ggplot2::ggplotGrob(alePlot)
              
            } #i
            
          } #j
          
        } #k

    }
      # plotOb$PDPPlots <- patchwork::wrap_plots(plotList
                                       # , axes = 'collect')& ylim(miny - 0.1, maxy + 0.1)
      # print(plotOb$PDPPlots)      
        
    
    
  } # End PDP Plots
  
### ---------------------- ALE Plots ------------------------------------------  
  
  # Moving on to ALE Plots 
  if(!is.null(options$ALE)&&options$ALE){
    print("ALE Plot")
    
  }
  
  
  
  
### ------------ All the covariance plots--------------------------------------
  if(!is.null(options$CovPlots)&&options$CovPlots){
    # Note that this is done across all outputs
    sdthresh <- 0.02 #This needs to be an argument!
    plotListC <- list()
    plotOb$Covar$Occurance <- list()
    plotOb$Covar$Effect <- list()
    plotOb$Covar$Change <- list()
    
    
    for(k in models){
      # for(i in Ob$Methodology$Data$YHeading){ # not required
      for(j in Ob$Methodology$Data$XHeadings){
        p_pd <- Ob$GMAM$Cov[[j]][[k]] %>%
          dplyr::filter(.data$sd >= sdthresh) 
          plotName <- paste(j,"-", k) 
          plotPoint <- paste0("PD",j,"_",k) 
          covarPlot <- ggplot2::ggplot(p_pd,
            ggplot2::aes(x = .data$cov_grid, y = .data$value, colour = .data$label)
          ) +
            ggplot2::geom_line() +
            ggplot2::geom_point() +
            ggplot2::theme(legend.position = c(0.80, 0.72),
                           legend.background = element_rect(fill = alpha(0.5))) +
            ggplot2::ylab("Prob of occurrence") +
            ggplot2::xlab(j) +
            #ggplot2::labs(colour = "Taxa") +
            ggplot2::ylim(0, 1) +
            # ggplot2::xlim(range(profiles_df$cov_grid))+
            ggplot2::ggtitle(plotName)
          
          plotOb$Covar$Occurance[[k]][[j]] <- ggpubr::as_ggplot(
            ggplot2::ggplotGrob(covarPlot))
          exportOb$Covar$Occurance[[k]][[j]] <- ggplot2::ggplotGrob(covarPlot)
      } #j
      # } #i
    } #k
    
    for(k in models){
      # for(i in Ob$Methodology$Data$YHeading){ # not required
      for(j in Ob$Methodology$Data$XHeadings){
        p_pd_avg <-  Ob$GMAM$Cov[[j]][[k]] %>%
          dplyr::filter(!is.na(.data$cov_grid))
        plotName <- paste(j,"-", k) 
        plotPoint <- paste0("Ave",j,"_",k) 
        effectPlot <- ggplot2::ggplot() +
          ggplot2::geom_line(
            data = p_pd_avg,
            ggplot2::aes(x = .data$cov_grid, y = .data$value, group = .data$label),
            alpha = 0.2
          ) +
          ggplot2::geom_line(
            data = p_pd_avg %>%
              dplyr::group_by(.data$cov_grid) %>%
              dplyr::summarise(
                mean = mean(.data$value)
              ),
            ggplot2::aes(x = .data$cov_grid, y = .data$mean)
          ) +
          ggplot2::labs(
            x = j,
            y = "Average effect"
          ) +
          ggplot2::theme_bw()+
          ggplot2::ylim(0, 1) +
          ggplot2::ggtitle(plotName)
        
        plotOb$Covar$Effect[[k]][[j]] <- ggpubr::as_ggplot(
          ggplot2::ggplotGrob(effectPlot))  
        exportOb$Covar$Effect[[k]][[j]] <- ggplot2::ggplotGrob(effectPlot)
        
      } #j
      # } #i
    } #k
    
    
    
    # if(!is.null(options$DerivativePlots)&&options$DerivativePlots){
      # plotListd <- list()
      for(k in models){
        # for(i in Ob$Methodology$Data$YHeading){
        for(j in Ob$Methodology$Data$XHeadings){
          p_pd_diff <- Ob$GMAM$Cov[[j]][[k]] %>%
            dplyr::filter(!is.na(.data$cov_diff))
          plotName <- paste(j,"-", k) 
          plotPoint <- paste0("Dev",j,"_",k) 
          changePlot <- ggplot2::ggplot(p_pd_diff,
                                                    ggplot2::aes(
                                                      x = .data$cov_grad,
                                                      group = .data$cov_grad,
                                                      y = .data$cov_diff
                                                    )
          ) +
            ggplot2::geom_boxplot() +
            ggplot2::theme_bw() +
            ggplot2::ylab("Change in prob") +
            ggplot2::xlab(j) +
            ggplot2::ylim(0, 1) +
            # ggplot2::xlim(range(profiles_df$cov_grid))+
            ggplot2::ggtitle(plotName)
          
          plotOb$Covar$Change[[k]][[j]] <- ggpubr::as_ggplot(
            ggplot2::ggplotGrob(changePlot))
          exportOb$Covar$Change[[k]][[j]] <- ggplot2::ggplotGrob(changePlot)
        } #j
        # } #i
      } #k
      
    # }
    
    
    ## Matching the importance plots
    # CovarPlots <- patchwork::wrap_plots(plotListC
    #                                          , axes = 'collect')  &
    #    ylim(0, 1)
    # plotOb$CovarPlots <- CovarPlots
    # # print(CovarPlots)    
  } # End Covar Plots section
  
### -------------------------- Importance Plots ----------------------------
    if(!is.null(options$ImportancePlots)&&options$ImportancePlots){
      local_plots <- list()
      
      global_plot <- ggplot2::ggplot(Ob$GMAM$VIP$Highest_VI$ModelStack)
      #the following line can be simplified later
      dft <- data.frame(t(Ob$GMAM$VIP$SD$ModelStack))
      #the following line should not be required checking of previous function required
      dft2 <- data.frame(name = attributes(Ob$GMAM$VIP$Order_VI$ModelStack)$names
                         , value = simplify2array(array(Ob$GMAM$VIP$Order_VI$ModelStack)))
      
      
      
      ## Global Plot is the importance across all responses
      global_plot <- ggplot(dft2, aes(x=reorder(name, value), y=value))+ 
        geom_bar(stat = "identity") +
        ggplot2::labs(y = "Importance", x = "Features") +         
        coord_flip()
      
      ## Local plots show the importance for each Response 
        # local_plots <- lapply(YHeadings,function(i){
      local_plots <- sapply(YHeadings,function(i){
          df_local<- data.frame(name = rownames(dft)[!is.na(dft[[i]])], value = dft[[i]][!is.na(dft[[i]])])
          lPlot <- ggplot(df_local, aes(x=reorder(name, value), y=value)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(y = "Importance", x = "", subtitle = i)  +
            coord_flip()
            
            ggpubr::as_ggplot(ggplot2::ggplotGrob(lPlot))
          
          
        },USE.NAMES = TRUE)
      
      local_exports <- sapply(YHeadings,function(i){
        df_local<- data.frame(name = rownames(dft)[!is.na(dft[[i]])], value = dft[[i]][!is.na(dft[[i]])])
        lPlot <- ggplot(df_local, aes(x=reorder(name, value), y=value)) +
          ggplot2::geom_boxplot() +
          ggplot2::labs(y = "Importance", x = "", subtitle = i)  +
          coord_flip()
        
        ggplot2::ggplotGrob(lPlot)
        
        
      },USE.NAMES = TRUE)
      
        
      l_VI_Plots <- patchwork::wrap_plots(local_plots)
      #p_VI_Plots
      
      gl_VI_Plots <- patchwork::wrap_plots(global_plot, l_VI_Plots)
      # print(gl_VI_Plots) 
      plotOb$Importance$Local <- local_plots  
      plotOb$Importance$Global <- ggpubr::as_ggplot(
        ggplot2::ggplotGrob(global_plot))
      
      exportOb$Importance$Local <- local_exports
      exportOb$Importance$Global <- ggplot2::ggplotGrob(global_plot)      
      # plotOb$Importance$Overall <- ggpubr::as_ggplot(
      #   ggplot2::ggplotGrob(gl_VI_Plots))
      
    }  
  
  
    
### ------------------------ Global Averaging Plots ------------------------- 
  # 
  # 
  # if(!is.null(options$GlobalAverage)&&options$GlobalAverage){
  #   for(k in models){
  #     for(i in Ob$Methodology$Data$YHeading){
  #       for(j in Ob$Methodology$Data$XHeadings){
  #         plotName <- paste(i, "-", k) 
  #         plotPoint <- paste0(i, "-", k) 
  #         miny <- min(miny,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
  #         maxy <- max(maxy,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
  #         plotList[[plotPoint]] <- plot(Ob$GMAM$LP_S[[i]][[k]][[j]])+
  #           ggtitle(plotName) ## Note this is still a PDP Plot
  #       } #j
  #     } #i
  #   } #k
  #   ALEPlots <- patchwork::wrap_plots(plotList
  #                                     , axes = 'collect')& ylim(miny - 0.1, maxy + 0.1)
  #   print(ALEPlots)    
  #   
  #   
  # }
  # 
  # # Moving on to Derivatives Plots 
  # 
  # if(!is.null(options$DerivativePlots)&&options$DerivativePlots){
  #   plotListd <- list()
  #     for(k in models){
  #       # for(i in Ob$Methodology$Data$YHeading){
  #         for(j in Ob$Methodology$Data$XHeadings){
  #           p_pd_diff <- Ob$GMAM$Cov[[j]][[k]] %>%
  #             dplyr::filter(!is.na(.data$cov_diff))
  #             plotName <- paste(j,"-", k) 
  #             plotPoint <- paste0(j,"_",k) 
  #             plotListC[[plotPoint]] <- ggplot2::ggplot(p_pd_diff,
  #               ggplot2::aes(
  #                 x = .data$cov_grad,
  #                 group = .data$cov_grad,
  #                 y = .data$cov_diff
  #               )
  #             ) +
  #             ggplot2::geom_boxplot() +
  #             ggplot2::theme_bw() +
  #             ggplot2::ylab("Change in prob") +
  #             ggplot2::xlab(j) +
  #             # ggplot2::ylim(0, 1) +
  #             ggplot2::xlim(range(profiles_df$cov_grid))+
  #             ggplot2::ggtitle(plotName)
  #         } #j
  #       # } #i
  #     } #k
  #   
  #   
  #   
  #       
  #   DerivativePlots <- patchwork::wrap_plots(plotListC
  #                                     , axes = 'collect')  #& ylim(miny - 0.1, maxy + 0.1)
  #   print(DerivativePlots)
  #   } # DerPlot
  #   

  
  
  plotOb$SummaryStatistics <- Ob$SummaryStatistics
  plotOb$Confusion <- Ob$Confusion
  exportOb$SummaryStatistics <- Ob$SummaryStatistics
  exportOb$Confusion <- Ob$Confusion
  ## This is a section for recording equations. Should probably move somewhere else?
  plotOb$Equations <- list()
  exportOb$Equations <- list()
  
  if(S$Methodology$Stacking$stackMode == "classification"){
    for(i in Ob$Methodology$Data$YHeading){
      plotOb$Equations[[i]] <- Ob$Models[[i]]$ModelStack$equations$class$.pred_class
      exportOb$Equations[[i]] <- Ob$Models[[i]]$ModelStack$equations$class$.pred_class
    }
  } else {
    for(i in Ob$Methodology$Data$YHeading){
      plotOb$Equations[[i]] <- Ob$Models[[i]]$ModelStack$equations$numeric$.pred
      exportOb$Equations[[i]] <- Ob$Models[[i]]$ModelStack$equations$numeric$.pred
    }    
    
  }
 
  if(options$Export){
    return(exportOb)
  } else {
    return(plotOb)
  }
}