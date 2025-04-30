#' Wrapper to plot mutlti-response model agnostic profile plots (partial
#' dependences and accumulated local effects).
#' 
#' @param profileData A data frame generated from [flashlight::light_profile()].
#' @param sdthresh A numeric value used to filter responses that are not
#' changing across the values of the feature (based on standard deviation).
#'
#' @details The aim of this function is to plot (1) a reduced set of response
#' variables that are responding to the feature of choice (plot 1) and the
#' average ALE of partial dependency  for all responses combined (plot 2). When
#' there are many responses plot 1 makes interpretation easier by focusing on
#' the responses changing the most with a feature. The feature selected and plot
#' type must be specified [flashlight::light_profile()] function.
#' 
#' @examples
#' library(tidymodels)
#' library(flashlight)
#' 
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#' 
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
#' fl <- mrFlashlight(
#'   mrIML_rf,
#'   response = "multi"
#' )
#'
#' profileData_pd <- light_profile(fl, v = "scale.prop.zos")
#' mrProfileplot(profileData_pd, sdthresh = 0.05)[[1]]
#' mrProfileplot(profileData_pd, sdthresh = 0.05)[[2]]
#' 
#' @export
mrProfileplot <- function(profileData,
                          sdthresh = 0.05) {
  b <- profileData$data
  feature <- names(b)[1]
  
  # Select only SNPs that are responding to this feature
  std <- b %>%
    dplyr::group_by(.data$label) %>%
    dplyr::summarise(sdALE = stats::sd(.data$value))
  
  Xred <- std %>%
    dplyr::filter(.data$sdALE >= sdthresh)
  
  redALE <- b %>%
    dplyr::filter(.data$label %in% Xred$label)
  
  profileData$data <- redALE
  
  redALEplot <- plot(profileData) +
    ggplot2::theme_bw()
  
  #-----------------------------------------------
  # Calculate global ALE
  
  if (length(unique(b[[feature]])) > 2) {
    GlobalPD <- b %>%
      ggplot2::ggplot(
        ggplot2::aes(x = .data[[feature]], y = .data$value)
      )+
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::ylab("Average effect") +
      ggplot2::theme_bw()
  } else {
    GlobalPD <- b %>%
      ggplot2::ggplot(
        ggplot2::aes(
          x = as.factor(.data[[feature]]),
          y = .data$value
        )
      ) +
      ggplot2::geom_boxplot() +
      ggplot2::labs(
        x = feature,
        y = "Average effect"
      ) +
      ggplot2::theme_bw()
  }
  
  list(
    redALEplot,
    GlobalPD
  )
}