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
#' there are many responses plot 1 makes interpretation easier by focussing on
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
#' @export
mrProfileplot <- function(profileData,
                          sdthresh = 0.05) {
  b <- profileData$data
  feature <- names(b[1])
  
  # select only SNPs that are responding to this featured
  std <- b %>%
    dplyr::group_by(label) %>%
    dplyr::summarise(sdALE = sd(value))

  Xred <- std %>%
    dplyr::filter(sdALE >= sdthresh)

  redALE <- b %>%
    dplyr::filter(label %in% Xred$label)

  profileData$data <- redALE

  redALEplot <- plot(profileData) +
    ggplot2::theme_bw()

  #-----------------------------------------------
  # Calculate global ALE

  if (lapply(b[1], is.factor) == FALSE) {
    b[1] <- apply(b[1], 2, as.factor)

    sum <- b %>%
      dplyr::group_by(b[1]) %>%
      dplyr::summarise(avgALE = mean(value))

    sum[1] <- sapply(sum[1], function(x) as.numeric(as.character(x)))

    sum$avgALE <- as.numeric(sum$avgALE)

    GlobalPD <- sum %>%
      ggplot2::ggplot(
        ggplot2::aes_string(feature, "avgALE")
      ) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::ylab("Average effect") +
      ggplot2::theme_bw()
  }

  if (lapply(b[1], is.factor) == TRUE) {
    GlobalPD <- b %>%
      ggplot2::ggplot(
        ggplot2::aes_string(feature, "value")
      ) +
      ggplot2::geom_boxplot(notch = TRUE) +
      ggplot2::theme_bw()
  }

  list(
    redALEplot,
    GlobalPD
  )

}
