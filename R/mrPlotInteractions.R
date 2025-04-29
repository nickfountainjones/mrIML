#' Plots global interactions as well as individual response interaction 
#' importance.
#' 
#' @details
#' 1st plot: Barplots showing the mean and cumulative importance of each of the
#' top pairs of interactions in the model.
#' 2nd plot: Barplot of the responses with the strongest interactions
#' 3rd plot: Barplots of the strongest interactions for each of the top response
#' variables.
#' 
#' @param interactions A data frame generated from [mrInteractions()]. 
#' @param Y A data frame of the response data set.
#' @param X A data frame of the feature data set.
#' @param top_ranking An integer value that determines how many of the strongest
#' feature interactions to view/include.
#' @param top_response An integer value for how many of the response variables
#' with the strongest interactions should be plotted view.
#' 
#' @examples !identical(Sys.getenv("NOT_CRAN"), "true")
#' library(tidymodels)
#'
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#'
#' # Specify a random forest tidy model
#' model_rf <- rand_forest(
#'   trees = 100, # 100 trees are set for brevity. Aim to start with 1000
#'   mode = "classification",
#'   mtry = tune(),
#'   min_n = tune()
#' ) %>%
#'   set_engine("randomForest")
#'
#' mrIML_rf <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = Y,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 5
#' )
#' 
#' mrIML_interactions_rf <- mrInteractions(
#'   mrIML_rf,
#'   num_bootstrap = 10,
#'   feature = "Plas"
#' )
#' 
#' #mrPlot_interactions(
#' # mrIML_interactions_rf,
#' # X,
#' # Y,
#' # top_ranking = 3,
#' # top_response = 3
#' #)
#' 
#' @export 
mrPlot_interactions <- function(interactions,
                                X,
                                Y,
                                top_ranking = 3,
                                top_response = 10) {
  
  n_features <- cbind(Y, X) %>%
    names() %>%
    length()
  
  variable_interactions <- as.data.frame(
    t(utils::combn(n_features, m = 2))
  ) %>% 
    tidyr::unite('variables', V1:V2, sep='*')
  
  colnames(interactions) <- names(Y)
  
  meanInteractions <- as.data.frame(rowMeans(interactions)) # Calculate average
  names(meanInteractions)[1] <- c('meanInt')
  
  sumInteractions <- as.data.frame(rowSums(interactions)) # Calculate sum
  names(sumInteractions)[1] <- c('sumInt')
  
  intData <- as.data.frame(interactions)
  intData <- cbind(
    variable_interactions,
    intData,
    meanInteractions,
    sumInteractions
  )
  
  inDataOrered <- intData %>%
    dplyr::arrange(.data$variables, .data$meanInt)
  
  inDataOrered_top <- inDataOrered[1L:top_ranking, ]
  
  p1 <- ggplot2::ggplot(
    inDataOrered_top,
    ggplot2::aes(y = .data$variables, x = .data$meanInt)
  ) + 
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Mean interaction importance",
      y = "Feature interactions"
    ) +
    ggplot2::geom_bar(
      stat = "identity"
    )
  
  p2 <- ggplot2::ggplot(
    inDataOrered_top,
    ggplot2::aes(y = .data$variables, x = .data$sumInt)
  ) + 
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Cumulative interaction importance",
      y = "Feature interactions"
    ) +
    ggplot2::geom_bar(stat = "identity")
  
  # Plotting both ensures that the cumulative score isn't 
  # Biased towards some strong interactions for some predictors
  p_cum_imp <- patchwork::wrap_plots(p1, p2, nrow = 1)
  
  # Select SNPS most affected by interactions for top 10 features
  MostImp <- as.data.frame(colSums(inDataOrered_top[-1]))
  names(MostImp) <- "sumInteract" 
  
  MostImp_t <-  MostImp %>% 
    t() %>% 
    as.data.frame()
  
  MostImp_t$meanInt <- NULL # Remove these stats as they are not needed
  MostImp_t$sumInt <- NULL
  
  MostImp_f <-  MostImp_t %>% 
    t() %>% 
    as.data.frame() %>% 
    tibble::rownames_to_column()
  
  MostImp_ordered <- MostImp_f %>% 
    dplyr::arrange(
      dplyr::desc(.data$sumInteract)
    )
  
  top_int_response <-  as.data.frame(MostImp_ordered[1L:top_ranking, ])
  
  p_strongest_int <- ggplot2::ggplot(
    top_int_response,
    ggplot2::aes(y = .data$rowname, x = .data$sumInteract)
  ) + 
    ggplot2::theme_bw() +
    ggplot2::labs(
      x = "Cumulative interaction importance",
      y = "Response"
    ) +
    ggplot2::geom_bar(stat = "identity")
  
  t_inDataOrered_top <- as.data.frame(t(inDataOrered_top)) %>% 
    janitor::row_to_names(row_number = 1) %>% 
    tibble::rownames_to_column()
  
  topIntC <- t_inDataOrered_top %>%
    dplyr::filter(.data$rowname %in% top_int_response$rowname)
  
  charvec <- as.data.frame(rep(topIntC$rowname, top_ranking))
  names(charvec) <- "Response"
  
  topIntC_plotData <- topIntC %>% 
    tidyr::gather(key = "rowname", value = "importance") %>% 
    dplyr::bind_cols(charvec)
  
  topIntC_plotData$importance <- as.numeric(topIntC_plotData$importance)
  
  p_strongest_int_resp <- topIntC_plotData %>%
    ggplot2::ggplot(
      ggplot2::aes(fill = .data$rowname, y = .data$importance, x = .data$rowname)
    ) + 
    ggplot2::geom_bar(position = "dodge", stat = "identity") +
    viridis::scale_fill_viridis(discrete = TRUE, option = "E") +
    ggplot2::facet_wrap(~.data$Response) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.x = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
      title = "Individual interaction models",
      fill = "Feature set"
    ) 
  
  list(
    p_cum_imp,
    p_strongest_int,
    p_strongest_int_resp
  )
}