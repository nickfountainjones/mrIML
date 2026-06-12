#' These are generic plots!


mrIMLStackPlot <- function(Ob, options = list()){
  if(length(options)==0){
    options$PDPPlot<-FALSE
    options$ALE <-FALSE
    options$CP <- FALSE
    options$DerivativePlots <- FALSE
    options$CovPlots <- FALSE
    options$StackOnly <- FALSE
  }
  plotOb <- list()

  #' This section is working on plots from the flashlight object.
  #' 
  #' Checks need to be done initially to check if the object has the flashlight objects
  #' already embedded. If not processing is skipped and an informative error should be
  #' returned.
  
  # Starting with the option of a Partial Derivative Plot section
  if(StackOnly){
    models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")
  } else {
    models <- c("ModelStack")
  }
  
  
  if(!is.null(options$PDPPlot)&&options$PDPPlot){
    
    if (!is.null(Ob$GMAM$LP_S[[1]]$ModelStack)) {
      plotList <- list()
      miscList <- list()
      miny <- 0.1
      maxy <- 0.2
      for(k in models){
        for(i in Ob$Methodology$Data$YHeading){
          for(j in Ob$Methodology$Data$XHeadings){
            plotName <- paste(i, "-", k) 
            plotPoint <- paste0(i, "-", k) 
            miny <- min(miny,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
            maxy <- max(maxy,Ob$GMAM$LP_S[[i]][[k]][[j]]$data$value)
            plotList[[plotPoint]] <- plot(Ob$GMAM$LP_S[[i]][[k]][[j]])+
              ggtitle(plotName)
          } #j
        } #i
      } #k
      

    }
      plotOb$PDPPlots <- patchwork::wrap_plots(plotList
                                       , axes = 'collect')& ylim(miny - 0.1, maxy + 0.1)
      print(plotOb$PDPPlots)      
          
    
    
  } # End PDP Plots
  
### ---------------------- ALE Plots ------------------------------------------  
  
  # Moving on to ALE Plots 
  if(!is.null(options$ALE)&&options$ALE){
    print("ALE Plot")
    
  }
  
  
  
  
### ------------ All the covariance plots--------------------------------------
  if(!is.null(options$CovPlots)&&options$CovPlots){
    sdthresh <- 0.05 #This needs to be an argument!
    plotListC <- list()
    for(k in models){
      # for(i in Ob$Methodology$Data$YHeading){ # not required
      for(j in Ob$Methodology$Data$XHeadings){
        p_pd <- Ob$GMAM$Cov[[j]][[k]] %>%
          dplyr::filter(.data$sd >= sdthresh) 
          plotName <- paste(j,"-", k) 
          plotPoint <- paste0("PD",j,"_",k) 
          plotListC[[plotPoint]] <- ggplot2::ggplot(p_pd,
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
        plotListC[[plotPoint]] <- ggplot2::ggplot() +
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
          ggplot2::ggtitle(plotName)
        
        
        
        
      } #j
      # } #i
    } #k
    
    
    
    # if(!is.null(options$DerivativePlots)&&options$DerivativePlots){
      plotListd <- list()
      for(k in models){
        # for(i in Ob$Methodology$Data$YHeading){
        for(j in Ob$Methodology$Data$XHeadings){
          p_pd_diff <- Ob$GMAM$Cov[[j]][[k]] %>%
            dplyr::filter(!is.na(.data$cov_diff))
          plotName <- paste(j,"-", k) 
          plotPoint <- paste0("Dev",j,"_",k) 
          plotListC[[plotPoint]] <- ggplot2::ggplot(p_pd_diff,
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
            # ggplot2::ylim(0, 1) +
            # ggplot2::xlim(range(profiles_df$cov_grid))+
            ggplot2::ggtitle(plotName)
        } #j
        # } #i
      } #k
      
    # }
    
    
    ## Matching the importance plots
    CovarPlots <- patchwork::wrap_plots(plotListC
                                             , axes = 'collect')  &
       ylim(0, 1)
    print(CovarPlots)    
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
        local_plots <- lapply(YHeadings,function(i){
          df_local<- data.frame(name = rownames(dft)[!is.na(dft[[i]])], value = dft[[i]][!is.na(dft[[i]])])
          ggplot(df_local, aes(x=reorder(name, value), y=value)) +
            ggplot2::geom_boxplot() +
            ggplot2::labs(y = "Importance", x = "", subtitle = i)  +
            coord_flip()
          
        })
      l_VI_Plots <- patchwork::wrap_plots(local_plots)
      #p_VI_Plots
      
      gl_VI_Plots <- patchwork::wrap_plots(global_plot, l_VI_Plots)
      print(gl_VI_Plots) 
      
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
  
  return(Ob) # do we want to return anything?
  
}