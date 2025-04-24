#' Calculate and visualize feature interactions
#'
#' This function calculates and visualizes interactions in the model using bootstrapping.
#' It provides overall, one-way, and two-way interactions for specified features.
#'
#' @param yhats A list of model predictions.
#' @param X The predictor data.
#' @param Y The response data.
#' @param num_bootstrap The number of bootstrap samples to generate (default: 1).
#' @param feature The feature for which interactions need to be calculated.
#' @param top.int The number of top interactions to display (default: 10).
#'
#' @return A list containing the visualizations for overall, one-way, and two-way interactions, as well as the interaction dataframes.
#' @export
#'
#' @examples
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
#' mrIML_interactions_rf[[1]]
#' mrIML_interactions_rf[[2]]
#' mrIML_interactions_rf[[3]]
#'   
mrInteractions <- function(mrIMLobj,
                           num_bootstrap = 1,
                           feature = NULL,
                           top_int = 10) {
  # Unpack mrIMLobj
  yhats <- mrIMLobj$Fits
  Y <- mrIMLobj$Data$Y
  X <- mrIMLobj$Data$X
  X1 <- mrIMLobj$Data$X1
  mode <- mrIMLobj$Model$mode
  
  # If no feature supplied default to first response variable
  if (is.null(feature)) feature <- names(yhats)[1]
  
  # Prepare the work for workers (multithreaded)
  response_vect <- rep(names(yhats), each = num_bootstrap)
  boot_vect <- rep(1:num_bootstrap, length(yhats))
  planned_work <- lapply(
    seq_along(response_vect),
    function(i) list(response = response_vect[i], boot = boot_vect[i])
  )
  
  # Define prediction function
  pred_fun <- function(m, dat) {
    predict(m, dat, type = "prob")[[".pred_1"]] # Only set up for "classification"?
  }
  
  # Calculate H statistics over responses and bootstraps (multithreaded)
  hstats_list <- future.apply::future_lapply(
    planned_work,
    function(p) {
      # prepare sample data
      bootstrap_sample <- yhats[[p$response]]$data
      if(num_bootstrap > 1) {
        bootstrap_sample <- bootstrap_sample %>%
          dplyr::slice(
            sample(1:dplyr::n(), replace = TRUE)
          )
      }
      
      # fit model
      wflow <- yhats[[p$response]]$last_mod_fit %>%
        tune::extract_workflow()
      wflow$data <- bootstrap_sample
      model_fit <- workflows::fit(wflow, data = bootstrap_sample)
      
      # Calculate H statistics
      s <- hstats::hstats(
        model_fit,
        v = names(bootstrap_sample)[-1],
        X = bootstrap_sample[-1],
        pred_fun = pred_fun,
        verbose = FALSE,
        pairwise_m = min(top_int, (ncol(bootstrap_sample) - 1))
      )
      
      list(
        response = p$response,
        boot = p$boot,
        H2 = hstats::h2(s) %>%
          mrIML:::h2_to_tibble() %>%
          dplyr::mutate(
            response = p$response,
            bstrap = p$boot
          ),
        H2_overall = hstats::h2_overall(s) %>%
          mrIML:::h2_to_tibble() %>%
          dplyr::mutate(
            response = p$response,
            bstrap = p$boot
          ),
        H2_pairwise = hstats::h2_pairwise(
          s,
          normalize = FALSE,
          squared = FALSE
        ) %>%
          mrIML:::h2_to_tibble() %>%
          dplyr::mutate(
            response = p$response,
            bstrap = p$boot
          )
      )
    },
    future.seed = TRUE
  )
  
  # Extract H statistics
  overall_int_df <- purrr::map(
    hstats_list,
    purrr::pluck("H2")
  ) %>%
    dplyr::bind_rows()
  
  overall_one_way_df <- purrr::map(
    hstats_list,
    purrr::pluck("H2_overall")
  ) %>%
    dplyr::bind_rows()
  
  overall_two_way_df <- purrr::map(
    hstats_list,
    purrr::pluck("H2_pairwise")
  ) %>%
    dplyr::bind_rows()
  
  # Plot comparisons
  p_overall <- overall_int_df %>%
    dplyr::rename(name = "response") %>%
    plot_hstat() +
    ggplot2::labs(
      title = "Overall interactions",
      x = "Response",
      y = "Overall"
    )
  
  p_one_way_filtered <- overall_one_way_df %>%
    dplyr::filter(response == feature) %>%
    plot_hstat() +
    ggplot2::labs(
      title = paste0(feature, " interaction contribution"),
      x = "Variable",
      y = "Overall interaction strength"
    )
  
  p_two_way_filtered <- overall_two_way_df %>%
    dplyr::filter(response == feature) %>%
    plot_hstat() +
    ggplot2::labs(
      title = paste0(feature, " pairwise interaction strengths"),
      x = "Interaction",
      y = "Interaction strength"
    )
  
  # Arrange output
  list(
    p_h2 = p_overall,
    p_h2_overall = p_one_way_filtered,
    p_h2_pairwise = p_two_way_filtered,
    h2_df = overall_int_df,
    h2_overall_df = overall_one_way_df,
    h2_pairwise_df = overall_two_way_df
  )
}
# Helper functions
h2_to_tibble <- function(h2_score) {
  h2_mat <- h2_score[["M"]]
  h2_vals <- drop(h2_mat)
  tibble::tibble(
    name = names(h2_vals),
    value = h2_vals
  )
}
plot_hstat <- function(hstat_df) {
  hstat_df %>%
    dplyr::group_by(name) %>%
    dplyr::summarise(
      mean_value = mean(value, na.rm = TRUE),
      ub_value = quantile(value, probs = c(0.95)),
      lb_value = quantile(value, probs = c(0.05))
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(x = reorder(name, -mean_value), y = mean_value)
    ) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = lb_value,
        ymax = ub_value
      ),
      width = 0.4
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}
