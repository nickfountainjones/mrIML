mrStackCovar <- function(Ob){
  
  Ob$Plots <-list()
  
  
  flLists <- list()
  mflS <- list()
  profiles_lpS <- list()
  profiles_dfS <- list()
  profiles_plots <- list()

  for (j in S$Methodology$Data$XHeadings){
    
    Ob$Plots$profile_plots[[j]] <- list()
    flLists[[j]] <- list()
    mfls[[j]] <- list()
    for (k in attributes(S$Methodology$Models)$name){
      Ob$Plots$profile_plots[[j]][[k]] <- list()
      flLists[[j]][[k]] <- list()
      mflS[[j]][[k]] <- flashlight::multiflashlight(flLists[[j]][[k]]
                                               , data = Ob$Data
                                               ,predict_function = pred_fun)
      for (i in  S$Methodology$Data$YHeadings){
        flLists[[j]][[k]][[i]] <- S$PD$FL[[i]][[k]]
        profiles_lpS[[k]] <- flashlight::light_profile(mflS[[k]], data = S$Data, v = j)
        profiles_dfS[[k]] <- profiles_lpS[[k]]$data %>%
          dplyr::rename(cov_grid = .data[[j]]) %>%
          dplyr::group_by(.data$label) %>%
          dplyr::mutate(
            sd = stats::sd(.data$value),
            cov_grad = .data$cov_grid +
              (0.5 * (dplyr::lead(.data$cov_grid) - .data$cov_grid)),
            cov_diff = abs(dplyr::lead(.data$value) - .data$value)
          )
        profiles_plots[[k]] <- profiles_dfS[[k]] %>%
          # dplyr::filter(.data$sd >= sdthresh) %>%
          ggplot2::ggplot(
            ggplot2::aes(x = .data$cov_grid, y = .data$value, colour = .data$label)
          ) +
          ggplot2::geom_line() +
          ggplot2::geom_point() +
          ggplot2::theme_bw() +
          ggplot2::ylab("Prob of occurrence") +
          ggplot2::xlab(var) +
          ggplot2::labs(colour = "Taxa") +
          ggplot2::ylim(0, 1) +
          ggplot2::xlim(range(profiles_df$cov_grid))+
          ggplot2::ggtitle(paste(k))
        
        
        
      }
    }
  }
  
  
  
  covarPlot <- patchwork::wrap_plots(profiles_plots
                                     , axes = 'collect')  
  
  
  
  
  
  
  
}
  
  