#' Bootstrap Partial Dependence plots
#'
#' This function bootstraps model predictions and generates partial dependence
#' plots for each response variable. It also creates a combined plot for the top
#' variables of interest.
#'
#' @param mrIML_obj A list object returned by [mrIMLpredicts()].
#' @param mrBootstrap_obj A list object returned by [mrBootstrap()].
#' @param vi_obj A list object returned by [mrvip()]. If `vi_obj` is not
#' provided then it is created inside `mrPD_bootstrap` by running [mrvip()]
#' @param target The target variable for generating plots.
#' @param global_top_var The number of top variables to consider (default: 2).
#' 
#' @return A list containing the partial dependence plots for each response
#' variable and a combined plot???
#'
#' @examples
#' library(tidymodels)
#' 
#' data <- MRFcov::Bird.parasites
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   dplyr::select(order(everything()))
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
#' mrIML_rf_boot <- mrIML_rf %>%
#'   mrBootstrap()
#'   
#' mrPD_bootstrap(
#'   mrIML_rf,
#'   mrIML_rf_boot,
#'   target = "Plas",
#'   global_top_var = 4
#' )
#' @export
mrPD_bootstrap <- function(mrIML_obj,
                           mrBootstrap_obj,
                           vi_obj = NULL,
                           target,
                           global_top_var = 2) {
  # Unpack mrIML_obj
  Y <- mrIML_obj$Data$Y
  X <- mrIML_obj$Data$X
  if (is.null(vi_obj)) {
    vi_obj <- mrIML_obj %>%
      mrIMLperformance()
  }
  
  n_response <- ncol(Y)
  complete_df <- cbind(Y, X)
  n_data <- ncol(complete_df)
  
  # Colapse bootstrap results into a dataframe
  pd_boot_df <- lapply(
    mrBootstrap_obj %>%
      purrr::flatten() %>%
      purrr::flatten(),
    function(pd_df) {
      names(pd_df)[1] <- "X"
      pd_df
    }
  ) %>%
  dplyr::bind_rows(.id = "var")
  # Filter to target (do this separately because need to output full df)
  pd_boot_df_target <- pd_boot_df %>%
    dplyr::filter(response == target)
  # Get list of vars to plot according to VI
  vi_obj <- vi_obj[[1]]
  important_vars <- vi_obj %>%
    dplyr::filter(response == target) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(mean_sd = mean(sd_value)) %>%
    dplyr::arrange(dplyr::desc(mean_sd)) %>%
    head(global_top_var) %>%
    dplyr::pull(var)
  # Create PD plots
  plot_list <- lapply(
    important_vars,
    function(v) {
      pd_var_df <- pd_boot_df_target %>%
        dplyr::filter(var == v)
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
    dplyr::group_by(bootstrap) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = X, y = value, group = bootstrap)
    ) +
    ggplot2::geom_line(alpha = 0.3) +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw()
}
plot_disc_pd <- function(pd_var_df, var_name, resp_name) {
  pd_var_df %>%
    ggplot2::ggplot(
      ggplot2::aes(x = ifelse(X == 1, "present", "absent"), y = value)
    ) +
    ggplot2::geom_boxplot() +
    ggplot2::labs(x = var_name, y = paste(resp_name, "prob")) +
    ggplot2::theme_bw() 
}
