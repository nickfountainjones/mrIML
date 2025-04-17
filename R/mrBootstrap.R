#' Bootstrap model predictions
#'
#' This function bootstraps model predictions and generates variable profiles
#' for each response variable based on the provided yhats.
#'
#' @param yhats A list of model predictions (e.g., from mrIMLpredicts).
#' @param num_bootstrap The number of bootstrap samples to generate (default: 10).
#' @param Y The response data.
#' @param mode \code{character}: 'classification' or 'regression' depending on the model type.
#' @param downsample Logical. Should the bootstrap samples be downsampled? (default: FALSE).
#' @return A list containing bootstrap samples of variable profiles for each response variable.
#' @export
#' @examples
#' \dontrun{
#' # Example usage:
#'
#' # Prepare response data
#' Y <- dplyr::select(Bird.parasites, -scale.prop.zos) %>%
#'   dplyr::select(sort(names(.))) # Response variables (e.g., SNPs, pathogens, species)
#'
#' # Example list of yhats generated from mrIMLpredicts (assume yhats_rf is defined)
#' yhats_rf <- mrIMLpredicts(...) # Replace with actual code to generate yhats_rf
#'
#' # Perform bootstrap analysis
#' bs_analysis <- mrBootstrap(yhats = yhats_rf, Y = Y, num_bootstrap = 50, mode = "classification")
#' }
mrBootstrap <- function(mrIMLobj,
                        num_bootstrap = 10,
                        downsample = FALSE) {
  
  yhats <- mrIMLobj$Fits
  Y <- mrIMLobj$Data$Y
  mode <- mrIMLobj$Model$mode
  #n_response <- length(yhats)
  
  # Set up flashlight functions for mode
  flashlight_ops <- mrIML_flashlight_setup(
    mode
  )
  
  # Determine downsampling freq
  if (downsample) {
    if (mode == "regression") {
      stop("Downsampling is not suitable for regression models.")
    }

    min_class_counts <- lapply(
      seq_along(Y),
      function(k) {
        Y[[k]] %>%
          table() %>%
          min()
      }
    )
  } else {
    min_class_counts <- lapply(
      seq_along(Y),
      function(k) NULL
    )
  }
  # Set up model and data to pass to future_lapply
  bootstrap_wf <- lapply(
    yhats,
    function(yhat) {
      data <- yhat$data
      wf <- yhat$last_mod_fit %>%
        tune::extract_workflow()
      list(
        data = data,
        workflow = wf
      )
    }
  )
  # Run bootstraps
  var_ids <- rep(1:length(bootstrap_wf), each = num_bootstrap)
  boot_ids <- rep(1:num_bootstrap, length(bootstrap_wf))
  
  pb <- txtProgressBar(min = 0, max = length(var_ids), style = 3)
  
  bootstrap_results <- future.apply::future_lapply(
    seq_along(var_ids),
    function(i, boot_fun) {
      setTxtProgressBar(pb, i)
      var_id <- var_ids[i]
      boot_id <- boot_ids[i]
      boot_fun(
        bootstrap_wf[[var_id]]$workflow,
        bootstrap_wf[[var_id]]$data,
        metrics = flashlight_ops$metrics,
        pred_fun = flashlight_ops$pred_fun,
        downsample_to = min_class_counts[[var_id]], # will be NULL if downsample = FALSE
        response_name = names(yhats)[var_id],
        boot_id = boot_id
      )
    },
    # Need to parse to workers for time being
    boot_fun = mrIML_internal_bootstrap_fun,
    future.seed = TRUE
  )
  
  # Organise in a list
  bstraps_pd_list <- lapply(
    yhats,
    function(i) vector("list", num_bootstrap)
  )
  for (i in seq_along(var_ids)) {
    bstraps_pd_list[[var_ids[i]]][[boot_ids[[i]]]] <- bootstrap_results[[i]]
  }

  bstraps_pd_list
}

mrIML_internal_bootstrap_fun <- function(wf,
                                         data,
                                         downsample,
                                         metrics,
                                         pred_fun,
                                         downsample_to = NULL,
                                         response_name,
                                         boot_id
                                         ) {
  # Resample data
  if (is.null(downsample_to)) {
    bootstrap_sample <- data %>%
      dplyr::slice(
        sample(1:nrow(data), replace = TRUE)
      )
  } else {
    classes <- unique(data[[1]])
    bootstrap_sample <- lapply(
      classes,
      function(cls) {
        data %>%
          dplyr::slice(
            sample(which(data[[1]] == cls), downsample_to, replace = TRUE)
          )
      }
    ) %>%
      dplyr::bind_rows()
  }
  
  # Refit model and run flashlight
  model_fit <- workflows::fit(wf, data = bootstrap_sample)
  fl <- flashlight::flashlight(
    model = model_fit,
    label = "class",
    data = bootstrap_sample,
    y = "class",
    predict_function = pred_fun,
    metrics = metrics
  )
  
  # Get light_profiles for all covariates
  pd_list <- lapply(
    names(bootstrap_sample)[-1],
    function(var_name) {
      pd_ <- flashlight::light_profile(
        fl,
        v = var_name
      )
      pd_$data %>%
        dplyr::mutate(
          bootstrap = boot_id,
          response = response_name
        )
    }
  )
  names(pd_list) <- names(bootstrap_sample)[-1]
  
  pd_list
}

