#' mrProfileplot:  Wrapper to plot mutlti-response model agnostic profile plots (partial dependences and accumulated local effects).
# .
#' @param profileData \code{dataframe} is the data generated from the flashlight packages 'light_profile' function.
#' @param 'sdthresh' \code{numeric} value used to filter responses that are not changing across the values of the feature (based on standard deviation)
#'
#' @details The aim of this function is to plot (1) a reduced set of response variables that are responding to the feature of choice (plot 1)
#' and the average ALE of partial dependency  for all responses combined (plot 2). When there are many responses plot 1 makes interpretation easier by focussing on the
#' the responses changing the most with a feature. The feature selected and plot type must be specified 'light_profile' function
#' @examples
#' flashlightObj <- mrFlashlight(yhats, X, Y, response = "multi")
#'
#' profileData_pd <- light_profile(flashlightObj, v = "Grassland") # partial dependencies
#' profileData_ale <- light_profile(flashlightObj, v = "Grassland", type = "ale") # acumulated local effects
#' mrProfileplot(profileData_pd, sdthresh = 0.05)
#' mrProfileplot(profileData_ale, sdthresh = 0.05)
#' @export

mrProfileplot <- function(profileData, sdthresh = 0.05) { # from mrFlashlight

  b <- profileData$data

  Feature <- names(b[1])

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

  print(redALEplot)

  readline(prompt = " Press [enter] to continue to the global summary plot.")

  #-----------------------------------------------
  # calculate global ALE

  # if(is.factor(b[1])==FALSE){

  if (lapply(b[1], is.factor) == FALSE) {
    b[1] <- apply(b[1], 2, as.factor)

    sum <- b %>%
      dplyr::group_by(b[1]) %>%
      dplyr::summarise(avgALE = mean(value))

    sum[1] <- sapply(sum[1], function(x) as.numeric(as.character(x)))

    sum$avgALE <- as.numeric(sum$avgALE)

    GlobalPD <- sum %>%
      ggplot2::ggplot(
        ggplot2::aes_string(Feature, "avgALE")
      ) +
      ggplot2::geom_smooth(method = "loess") +
      ggplot2::ylab("Average effect") +
      ggplot2::theme_bw()
  }

  if (lapply(b[1], is.factor) == TRUE) {
    GlobalPD <- b %>%
      ggplot2::ggplot(
        ggplot2::aes_string(Feature, "value")
      ) +
      ggplot2::geom_boxplot(notch = TRUE) +
      ggplot2::theme_bw()
  }

  print(GlobalPD)
}
