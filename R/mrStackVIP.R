#' Calculates and helps interpret variable importance across models


mrStackVIP <- function(Ob, bootstrapping = FALSE){
  
  

  
  #Something needs to be done about boostrapping
  
  ## Collating the different light profiles:  
  # Split workflow depending on if mrBootstrap_obj is supplied
  if (bootstrapping) {
    Ob <- mrVIP_StackBootOb(Ob)
  } else {
    Ob <- mrVIP_StackOb(Ob)
  }
  
  ## Setting up MVPs:
  
  # # Reset global_ and local_top_var to the total number of variables if needed
  # global_top_var <- ifelse(
  #   global_top_var > ncol(c(S$Methodology$Data$XHeadings, S$Methodology$Data$X1Headings)),
  #   ncol(c(S$Methodology$Data$XHeadings, S$Methodology$Data$X1Headings)),
  #   global_top_var
  # )
  # local_top_var <- ifelse(
  #   local_top_var > ncol(cbind(X, X1)),
  #   ncol(cbind(X, X1)),
  #   local_top_var
  # )
  # 


  
  

  return(Ob)  
}



mrVIP_StackOb <- function(Ob){
  sd_tab <- data.frame() 

  models <- c(attributes(Ob$Methodology$Models)$name,"ModelStack")

  for (i in Ob$Methodology$Data$YHeadings){
    for (k in models){

      Ob$GMAM$VIP[[i]][[k]] <- list()
      for (X1 in attributes(Ob$GMAM$LP_S[[i]][[k]])$names){
        tempVar <- data.frame(var = X1,
          sd_value = stats::sd(Ob$GMAM$LP_S[[i]][[k]][[X1]]$data$value),
          bootstrap = NA)
          Ob$GMAM$VIP[[i]][[k]] <- rbind(Ob$GMAM$VIP[[i]][[k]], tempVar)
      } #X1 
      
    } #k
  } #i
# # Legacy to match orientation and type of mrVIP
#   for (k in models){
#     sdTable <- data.frame(matrix(nrow = 0, ncol = length(c(Ob$Methodology$Data$XHeadings,Ob$Methodology$Data$YHeadings))))
#     colnames(sdTable) <- c(Ob$Methodology$Data$XHeadings,Ob$Methodology$Data$YHeadings)
#     for (i in Ob$Methodology$Data$YHeadings){
#       
#       sdRow <- data.frame(t(c(mean(Ob$GMAM$VIP[[i]][[k]]$sd_value),Ob$GMAM$VIP[[i]][[k]]$sd_value)))
#       colnames(sdRow) <- c(i, Ob$GMAM$VIP[[i]][[k]]$var)
#       row.names(sdRow) <- i
#       sdTable <- rbind(sdTable, sdRow)
#     } #i
#     Ob$GMAM$VIP$Legacy$SD[[k]] <- sdTable
#   } #k

  for (k in models){
    sdTable <- data.frame(matrix(nrow = 0, ncol = length(c(Ob$Methodology$Data$XHeadings,Ob$Methodology$Data$YHeadings))))
    colnames(sdTable) <- c(Ob$Methodology$Data$XHeadings,Ob$Methodology$Data$YHeadings)
    for (i in Ob$Methodology$Data$YHeadings){
      
      sdRow <- data.frame(t(c(NA,Ob$GMAM$VIP[[i]][[k]]$sd_value)))
      colnames(sdRow) <- c(i, Ob$GMAM$VIP[[i]][[k]]$var)
      row.names(sdRow) <- i
      sdTable <- rbind(sdTable, sdRow)
    } #i
    Ob$GMAM$VIP$SD[[k]] <- sdTable
  } #k

  #Ordered importance section:
  sdMeans <- list()  
  for (k in models){
    
    for (X1 in c(Ob$Methodology$Data$XHeadings, Ob$Methodology$Data$X1Headings)){
      sdMeans[[k]][[X1]]= mean(Ob$GMAM$VIP$SD[[k]][[X1]], na.rm = TRUE)
      
    } #i
    #print(sdMeans)
    # orderedMeans[[k]][[X1]] <- sdMeans[[k]][order(t(data.frame(sdMeans[[k]])), decreasing = TRUE, na.last = TRUE)]
    sdMeans[[k]]<- sdMeans[[k]][order(t(data.frame(sdMeans[[k]])), decreasing = TRUE, na.last = TRUE)]
    
    
    tempRow <- t(sdMeans)
    
    
    # row.names(sdMeans) <- k
  } #k
  # Ob$GMAM$VIP$Highest_VI <- t(orderedMeans)
  Ob$GMAM$VIP$Order_VI <- sdMeans


  
  
  

  # Ob$GMAM$VIP$TempTab <- rawTable
  return(Ob)
}


mrVIP_StackBootOb <- function(Ob){
  this_does_nothing <- TRUE
  
  return(Ob)
  
  
}
