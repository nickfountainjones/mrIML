#' Calculate and visualize feature interactions
#'
#' A wrapper around [hstats::hstats()]. Calculates and visualizes H-statistics
#' for interactions in the model using bootstrapping. See `help("hstats")` for
#' details on H-statistics.
#'
#' @param mrIMLobj A list object output by [mrIMLpredicts()].
#' @param num_bootstrap The number of bootstrap samples to generate
#' (default: 1).
#' @param feature The response model for which detailed interaction plots should 
#' be generated.
#' @param top_int The number of top interactions to display (default: 10).
#'
#' @return A list containing:
#' * `$p_h2`: An ordered bar plot of the variability in each response model that
#' is unexplained by the main effects.
#' * `$p_h2_overall`: An ordered bar plot of the percentage of prediction
#' variability that can be attributed to interactions with each predictor for the
#' model specified by `feature`.
#' * `$p_h2_pairwise`: An ordered bar plot of the strength of the two-way
#' interactions in the model specified by `feature`. The strength of an
#' interaction is taken to be the un-normalized square root of the
#' H2-pairwise statistic (which is on the prediction scale).
#' * `$h2_df`: A data frame of the H2 statistics for each response model, along
#' with bootstraps if applicable.
#' * `$h2_overall_df`: A data frame of the H2-overall statistics for the variable
#' in each response model, along with bootstraps if applicable.
#' * `$h2_pairwise_df`: A data frame of the H2-pairwise statistics for the
#' variable in each response model, along with bootstraps if applicable.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' mrIML_rf <- mrIML::mrIML_bird_parasites_RF
#' 
#' mrIML_interactions_rf <- mrInteractions(
#'   mrIML_rf,
#'   num_bootstrap = 50,
#'   feature = "Plas"
#' )
#'
#' mrIML_interactions_rf[[1]]
#' mrIML_interactions_rf[[2]]
#' mrIML_interactions_rf[[3]]
#'
#' @export
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
    stats::predict(m, dat, type = "prob")[[".pred_1"]] # Only "classification"?
  }
  
  # Calculate H statistics over responses and bootstraps (multithreaded)
  hstats_list <- future.apply::future_lapply(
    planned_work,
    function(p) {
      # prepare sample data
      bootstrap_sample <- yhats[[p$response]]$data
      if (num_bootstrap > 1) {
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
          h2_to_tibble() %>%
          dplyr::mutate(
            response = p$response,
            bstrap = p$boot
          ),
        H2_overall = hstats::h2_overall(s) %>%
          h2_to_tibble() %>%
          dplyr::mutate(
            response = p$response,
            bstrap = p$boot
          ),
        H2_pairwise = hstats::h2_pairwise(
          s,
          normalize = FALSE,
          squared = FALSE
        ) %>%
          h2_to_tibble() %>%
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
      x = "Response model",
      y = "Overall"
    )
  
  p_one_way_filtered <- overall_one_way_df %>%
    dplyr::filter(.data$response == feature) %>%
    plot_hstat() +
    ggplot2::labs(
      title = paste0(feature, " one-way interactions"),
      x = "Variable",
      y = "Interaction importance"
    )
  
  p_two_way_filtered <- overall_two_way_df %>%
    dplyr::filter(.data$response == feature) %>%
    plot_hstat() +
    ggplot2::labs(
      title = paste0(feature, " tow-way interactions"),
      x = "Interaction",
      y = "Interaction importance"
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
    dplyr::group_by(.data$name) %>%
    dplyr::summarise(
      mean_value = mean(.data$value, na.rm = TRUE),
      ub_value = stats::quantile(.data$value, probs = 0.95),
      lb_value = stats::quantile(.data$value, probs = 0.05),
      .groups = "drop"
    ) %>%
    ggplot2::ggplot(
      ggplot2::aes(
        x = stats::reorder(.data$name, -.data$mean_value),
        y = .data$mean_value
      )
    ) +
    ggplot2::geom_col() +
    ggplot2::geom_errorbar(
      ggplot2::aes(
        ymin = .data$lb_value,
        ymax = .data$ub_value
      ),
      width = 0.4
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)
    )
}