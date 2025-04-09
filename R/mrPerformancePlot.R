#' Plot Model Performance Comparison
#'
#' Create visualizations to compare the performance of two models based on their performance metrics.
#'
#' @param ModelPerf1 Dataframe of model performance metrics for the first model to compare.
#' @param ModelPerf2 Dataframe of model performance metrics for the second model to compare.
#' @param mode Character string indicating whether the mode is 'classification' or 'regression'. Default is 'classification'.
#' @return A list containing:
#' \item{p1}{A ggplot object for the boxplot of model performance metrics.}
#' \item{p2}{A ggplot object for the barplot of differences in performance metrics.}
#' \item{wide_df}{A dataframe with the wide format of model performance metrics and their differences.}
#' @export
#' @examples
#' plots <- mrPerformancePlot(ModelPerf1 = ModelPerf_lm, ModelPerf2 = ModelPerf_rf, mod_names = c("linear_reg", "rand_forest"), mode = "regression")
#'
mrPerformancePlot <- function(ModelPerf1 = NULL,
                              ModelPerf2 = NULL,
                              mode = "classification") {
  # Extract performance data frames
  model1_df <- ModelPerf1[[1]]
  model2_df <- ModelPerf2[[1]]
  
  # Check that the two data frames match 
  if (!identical(dim(model1_df), dim(model2_df)) |
      !identical(model1_df$response, model2_df$response)) {
    stop(
      "mrIMLperformance objects must be fit to the same data.",
      call. = FALSE
    )
  }
  
  # Get model names for plotting
  mod_names <- c(
    model1_df$model_name %>%
      unique(),
    model2_df$model_name %>%
      unique()
  )
  
  # Set the performance metric
  if (mode == "classification") {
    metric_name <- "mcc"
  } else if (mode == "regression") {
    metric_name <- "rmse"
  } else {
    stop("Mode must be either regression or classification.", call. = FALSE)
  }
  
  # detect outliers function
  findoutlier <- function(x) {
    (x < (quantile(x, .25) - 1.5 * IQR(x))) | 
      (x > (quantile(x, .75) + 1.5 * IQR(x)))
  }  
  
  model_compare_df <- lapply(
    list(model1_df, model2_df),
    function(df) {
      df %>%
        dplyr::rename(metric = metric_name) %>%
        dplyr::mutate(outlier = ifelse(findoutlier(metric), metric, NA))
    } 
  ) %>%
    dplyr::bind_rows()
  
  # Create boxplot of model performance metrics
  p1 <- model_compare_df %>%
    dplyr::group_by(model_name) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = model_name,
        y = metric,
        label = round(outlier, 4)
      )
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::geom_text(
      na.rm = TRUE, hjust = -0.5
    ) +
    ggplot2::theme_bw() +
    ggplot2::labs(y = toupper(metric_name))
  

  # Data frame of ndividual taxa
  wide_df <- model_compare_df %>%
    dplyr::select(response, model_name, metric, outlier) %>%
    tidyr::pivot_wider(names_from = model_name, values_from = metric) %>%
    # Calculate differences from the first model (combined)
    dplyr::mutate(
      diff_mod1_2 = .data[[mod_names[2]]] - .data[[mod_names[1]]]
    )

  # Reshape back to long format for plotting
  long_df <- wide_df %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("diff_"),
      names_to = "comparison",
      values_to = "difference"
    )

  # Create bar plot of differences in performance metrics 
  p2 <- long_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        y = reorder(response, difference, decreasing = T),
        x = difference
      )
    ) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(
      y = "Response",
      x = paste0("Difference in ", toupper(metric_name)),
      title = paste0(mod_names[2], " vs ", mod_names[1])
    ) +
    ggplot2::theme_bw()

  list(
    performance_plot = p1,
    performance_diff_plot = p2,
    performance_diff_df = wide_df
  )
}
