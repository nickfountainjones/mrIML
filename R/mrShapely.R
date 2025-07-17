#' Generate SHAP (SHapley Additive exPlanations) Plots for Multiple Models
#' and Responses
#'
#' This function generates SHAP (SHapley Additive exPlanations) plots for
#' multiple models and responses.
#'
#' @param mrIML_obj A list object output by [mrIMLpredicts()].
#' @param taxa An optional character vector specifying which responses to include.
#' @param kind A character string passed to [shapviz::sv_importance()] specifying
#' the type of plot parameter (e.g., "beeswarm" for feature effect plot, "bar" for
#' variable importance plot, or "both").
#' @param max_display An integer passed to [shapviz::sv_importance()] specifying
#' the maximum number of features to display.
#' @param plot_feature_effects A logical indicating whether to generate feature
#' effect plots (default is TRUE).
#' @param plot_dependencies A logical indicating whether to generate dependency
#' plots (default is TRUE).
#' @param plot_2D_dependencies A logical indicating whether to generate
#' interaction plots (default is TRUE).
#'
#' @return A list object where the first element returns the SHAP results, and the
#' following elements contain the feature-effect, 1D-dependencies, and 2D-dependencies
#' if they were set to TRUE in the input.
#'
#' @examplesIf identical(Sys.getenv("NOT_CRAN"), "true")
#' mrIML_model <- mrIML::mrIML_bird_parasites_RF
#' 
#' mrShapely(mrIML_model, plot_2D_dependencies = FALSE)
#' @export
mrShapely <- function(mrIML_obj,
                      taxa = NULL,
                      kind = "beeswarm",
                      max_display = 15L,
                      plot_feature_effects = TRUE,
                      plot_dependencies = TRUE,
                      plot_2D_dependencies = TRUE){
  # Requiest to download SHAP packages...
  rlang::check_installed("kernelshap", reason = "to use `mrShapely()`")
  rlang::check_installed("shapviz", reason = "to use `mrShapely()`")
  # Filter to taxa if supplied
  if (!is.null(taxa)) {
    all_taxa <- names(mrIML_obj$Fits)
    mrIML_obj$Fits <- mrIML_obj$Fits[all_taxa %in% taxa]
  }
  # Set mode
  mode <- mrIML_obj$Model$mode
  # Calculate shapleys for each response
  shap_list <- future.apply::future_lapply(
    mrIML_obj$Fits,
    function(response){
      # Prepare arguments to pass to kernelshap()
      shap_args <- switch(
        mode,
        # Classification models need to predict the probability (type = "prob").
        classification = list(
          object = response$last_mod_fit %>%
            hardhat::extract_workflow(),
          X = response$data %>%
            dplyr::select(-class),
          bg_X = response$data,
          type = "prob"
        ),
        # Regression models need to predict the raw response (defult type).
        regression = list(
          object = response$last_mod_fit %>%
            hardhat::extract_workflow(),
          X = response$data %>%
            dplyr::select(-class),
          bg_X = response$data
        )
      )
      # Call kernelshap() with arguments from above
      shap_obj <- do.call(kernelshap::kernelshap, shap_args) %>%
        shapviz::shapviz()

      shap_obj
    },
    future.seed = TRUE
  )
  # Plot shap feature effects
  if (plot_feature_effects) {
    feature_eff_plots <- purrr::map2(
      .x = shap_list, .y = names(shap_list),
      function(shap_obj, response_name) {
        shap_obj %>%
          shapviz::sv_importance(
            kind = kind,
            max_display = max_display
          ) +
          patchwork::plot_annotation(response_name)
      }
    )    
  } else {
    feature_eff_plots <- NULL
  }
  # Plot shap dependancy
  if (plot_dependencies) {
    dependancy_plots <- purrr::map2(
      .x = shap_list, .y = names(shap_list),
      function(shap_obj, response_name) {
        covariate_names <- mrIML_obj$Fits[[response_name]]$data %>%
          dplyr::select(-class) %>%
          colnames()
        purrr::map(
          covariate_names,
          function(cov_name) {
            shap_obj %>%
              shapviz::sv_dependence(
                v = cov_name
            ) +
              patchwork::plot_annotation(response_name)
          }
        )
      }
    )
  } else {
    ependancy_plots <- NULL
  }
  # Plot interaction effects
  if (plot_2D_dependencies) {
    dependancy2D_plots <- purrr::map2(
      .x = shap_list, .y = names(shap_list),
      function(shap_obj, response_name) {
        covariate_names <- mrIML_obj$Fits[[response_name]]$data %>%
          dplyr::select(-class) %>%
          colnames()
        covariate_name_combos <- utils::combn(
          covariate_names,
          m = 2,
          simplify = FALSE
        )
        purrr::map(
          covariate_name_combos,
          function(cov_comb_names) {
            shap_obj %>%
              shapviz::sv_dependence2D(
                x = cov_comb_names[1],
                y = cov_comb_names[2]
            ) +
              patchwork::plot_annotation(response_name)
          }
        )
      }
    )
  } else {
    dependancy2D_plots <- NULL
  }
  # Retrun a list of plots
  list(
    SHAP_values = shap_list,
    feature_effects = feature_eff_plots,
    dependencies = dependancy_plots,
    dependencies2D = dependancy2D_plots
  ) %>%
    purrr::compact() # Remove any NULL objects in list
}
