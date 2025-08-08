#' Bootstrap Partial Dependence Plots
#'
#' This function extracts and plots the bootrapped partial dependence functions
#' calculated by [mrBootstrap()] for each response variable.
#'
#' @param mrIML_obj A list object returned by [mrIMLpredicts()].
#' @param mrBootstrap_obj A list object returned by [mrBootstrap()].
#' @param vi_obj A list object returned by [mrVip()]. If `vi_obj` is not
#' provided, then it is created inside `mrPD_bootstrap` by running [mrVip()].
#' @param target The target variable for generating plots.
#' @param global_top_var The number of top variables to consider (default: 2).
#'
#' @return A list with two elements:
#' * `[[1]]`: A data frame of the partial dependence grid for each response model,
#' predictor variable, and bootstrap.
#' * `[[2]]`: A list of partial dependence plots for each predictor variable in
#' the `target` response model.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' mrIML_rf <- mrIML::mrIML_bird_parasites_RF
#'
#' mrIML_rf_boot <- mrIML_rf %>%
#'   mrBootstrap(num_bootstrap = 50)
#'
#' mrIML_rf_PD <- mrPdPlotBootstrap(
#'   mrIML_rf,
#'   mrIML_rf_boot,
#'   target = "Plas",
#'   global_top_var = 4
#' )
#'
#' head(mrIML_rf_PD[[1]])
#' mrIML_rf_PD[[2]]
#'
#' @export
mrPdPlotBootstrap <- function(
  mrIML_obj,
  mrBootstrap_obj,
  vi_obj = NULL,
  target,
  global_top_var = 2
) {
  # Unpack mrIML_obj
  Y <- mrIML_obj$Data$Y
  X <- mrIML_obj$Data$X
  if (is.null(vi_obj)) {
    vi_obj <- mrVip(
      mrIMLobj = mrIML_obj,
      mrBootstrap_obj = mrBootstrap_obj
    )
  }

  n_response <- ncol(Y)
  complete_df <- cbind(Y, X)
  n_data <- ncol(complete_df)

  # Collapse bootstrap results into named list of dataframe
  pd_boot_df <- lapply(
    mrBootstrap_obj %>%
      purrr::flatten() %>%
      purrr::flatten(),
    function(pd_df) {
      names(pd_df)[1] <- "X"
      pd_df
    }
  )

  # Filter list to target response
  pd_boot_df_target <- pd_boot_df %>%
    purrr::keep(~ unique(.$response) == target)

  # Get list of vars to plot according to VI
  important_vars <- vi_obj[[1]] %>%
    dplyr::filter(.data$response == target) %>%
    dplyr::group_by(.data$var) %>%
    dplyr::summarise(mean_sd = mean(.data$sd_value), .groups = "drop") %>%
    dplyr::arrange(dplyr::desc(.data$mean_sd)) %>%
    utils::head(global_top_var) %>%
    dplyr::pull(.data$var)

  # Create PD plots
  plot_list <- lapply(
    important_vars,
    function(v) {
      pd_var_df <- pd_boot_df_target[[v]]
      if (length(unique(pd_var_df$X)) == 2) {
        plot_disc_pd(pd_var_df, v, target)
      } else {
        plot_cont_pd(pd_var_df, v, target)
      }
    }
  )

  # Plot in grid
  list(
    pd_boot_df,
    patchwork::wrap_plots(plot_list)
  )
}

plot_cont_pd <- function(pd_var_df, var_name, resp_name) {
  pd_var_df %>%
    dplyr::group_by(.data$bootstrap) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = .data$X, y = .data$value, group = .data$bootstrap)
    ) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw()
}

plot_disc_pd <- function(pd_var_df, var_name, resp_name) {
  pd_var_df %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = ifelse(.data$X == 1, "present", "absent"),
        y = .data$value
      )
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw()
}
