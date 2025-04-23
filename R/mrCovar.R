#' Investigate partial dependencies of a covariate for mrIML JSDMs (Joint
#' species distirbution models)
#'
#' This function is a wrapper around [mrFlashlight()] that plots the covariate
#' partial dependencies for a specified environmental/host variable. It also
#' filters the taxa based on standard deviation thresholds.
#'
#' @param mrIMLobj A list object output by [mrIMLpredict()].
#' @param var The variable of interest for calculating the profile.
#' @param sdthresh The standard deviation threshold for filtering taxa
#' (default: 0.05).
#' 
#' @return A plot displaying the covariate partial dependence profiles for those
#' models that meet the `sdthreshold` requirement and another summarizing the
#' rates of change in probability for the specified variable
#' (the derivatives of the PD curves).
#' 
#' @examples
#' library(tidymodels)
#' 
#' # Without bootstrap
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
#' mrIML_rf %>%
#'   mr_Covar(var = "scale.prop.zos", sdthresh = 0.05)
#' 
#' @export
mr_Covar <- function(mrIMLobj,
                     var, 
                     sdthresh = 0.05) {
  # Unpack mrIMLobj
  yhats <- mrIMLobj$Fits
  Y <- mrIMLobj$Data$Y
  X <- mrIMLobj$Data$X
  X1 <- mrIMLobj$Data$X1
  mode <- mrIMLobj$Model$mode
  
  # Run flashlifht
  profiles_fl <- mrFlashlight(
    mrIMLobj,
    response = "multi"
  ) %>%
    flashlight::light_profile(var)
  
  # Filter results and get derivatives
  profiles_df <- profiles_fl$data %>%
    dplyr::rename(cov_grid = var) %>%
    dplyr::group_by(label) %>%
    mutate(
      sd = sd(value),
      cov_grad = cov_grid + (0.5 * (lead(cov_grid) - cov_grid)),
      cov_diff = abs(lead(value) - value)
    ) %>%
    dplyr::filter(
      sd >= sdthresh
    )
  
  p_pd <- profiles_df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = cov_grid, y = value, colour = label)
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Prob of occurance") +
    ggplot2::xlab(var) +
    ggplot2::labs(colour = "Taxa") +
    ggplot2::ylim(0, 1) +
    ggplot2::xlim(range(profiles_df$cov_grid))
  
  p_pd_diff <- profiles_df %>%
    dplyr::filter(!is.na(cov_diff)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = cov_grad, group = cov_grad, y = cov_diff)
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Change in prob") +
    ggplot2::xlab(var) +
    ggplot2::ylim(0, 1) +
    ggplot2::xlim(range(profiles_df$cov_grid))
  
  patchwork::wrap_plots(p_pd, p_pd_diff, ncol = 1, axis_titles = "collect")
}
