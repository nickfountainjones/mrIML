#' Investigate partial dependencies of a covariate for mrIML JSDMs (Joint
#' species distirbution models)
#'
#' This function is a wrapper around [mrFlashlight()] that plots the covariate
#' partial dependencies for a specified environmental/host variable. It also
#' filters the taxa based on standard deviation thresholds.
#'
#' @param mrIMLobj A list object output by [mrIMLpredicts()].
#' @param var The variable of interest for calculating the profile.
#' @param sdthresh The standard deviation threshold for filtering taxa
#' (default: 0.05).
#' 
#' @return A list of figures:
#' * `$partial_dep_curves`: The covariate partial dependence profiles for those
#' models that meet the `sdthreshold` requirement
#' * `$partial_dep_avg`: The average partial dependence profile for all models.
#' All indervidual model partial dependence profiles are silhouetted in the
#' background.
#' * `$partial_dep_diff`: The distribution of the rates of change in probability
#' for the specified variable (the derivatives of the PD curves). Useful to
#' identify key threshold values in the variable.
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
  
  # Run flashlight
  profiles_fl <- mrFlashlight(
    mrIMLobj,
    response = "multi"
  ) %>%
    flashlight::light_profile(var)
  
  # Filter results and get derivatives
  profiles_df <- profiles_fl$data %>%
    dplyr::rename(cov_grid = .data[[var]]) %>%
    dplyr::group_by(.data$label) %>%
    dplyr::mutate(
      sd = stats::sd(.data$value),
      cov_grad = .data$cov_grid +
        (0.5 * (dplyr::lead(.data$cov_grid) - .data$cov_grid)),
      cov_diff = abs(dplyr::lead(.data$value) - .data$value)
    )
  
  # Plot partial dependence
  p_pd <- profiles_df %>%
    dplyr::filter(.data$sd >= sdthresh) %>%
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
    ggplot2::xlim(range(profiles_df$cov_grid))
  
  # Plot global average
  p_pd_avg <- ggplot2::ggplot() +
    ggplot2::geom_line(
      data = profiles_df,
      ggplot2::aes(x = .data$cov_grid, y = .data$value, group = .data$label),
      alpha = 0.2
    ) +
    ggplot2::geom_line(
      data = profiles_df %>%
        dplyr::group_by(.data$cov_grid) %>%
        dplyr::summarise(
          mean = mean(.data$value)
        ),
      ggplot2::aes(x = .data$cov_grid, y = .data$mean)
    ) +
    ggplot2::labs(
      x = var,
      y = "Average effect"
    ) +
    ggplot2::theme_bw()
  
  # Plot derivatives
  p_pd_diff <- profiles_df %>%
    dplyr::filter(!is.na(.data$cov_diff)) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$cov_grad, group = .data$cov_grad, y = .data$cov_diff)
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::theme_bw() +
    ggplot2::ylab("Change in prob") +
    ggplot2::xlab(var) +
    ggplot2::ylim(0, 1) +
    ggplot2::xlim(range(profiles_df$cov_grid))
  
  list(
    p_pd,
    p_pd_avg,
    p_pd_diff
  )
}