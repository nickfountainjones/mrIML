#' Bootstrap mrIML model predictions
#'
#' This function bootstraps model predictions and generates variable profiles
#' for each response variable.
#'
#' @param mrIMLobj A list object output by [mrIMLpredicts()].
#' @param num_bootstrap The number of bootstrap samples to generate
#' (default: 10).
#' @param downsample Logical. Should the bootstrap samples be downsampled?
#' (default: FALSE).
#'
#' @return A list containing bootstrap samples of variable profiles for each
#' response variable.
#' 
#' @examplesIf !identical(Sys.getenv("NOT_CRAN"), "true")
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
#' #future::plan(multisession, workers = 4)
#'
#' mrIML_bootstrap <- mrIML_rf %>%
#'   mrBootstrap(
#'     num_bootstrap = 50
#'   )
#'
#' @export
mrBootstrap <- function(mrIMLobj,
                        num_bootstrap = 10,
                        downsample = FALSE) {
  
  yhats <- mrIMLobj$Fits
  Y <- mrIMLobj$Data$Y
  mode <- mrIMLobj$Model$mode
  
  # Set up flashlight functions for mode
  flashlight_ops <- mrIML_flashlight_setup(mode)
  
  # Determine downsampling frequency
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
  
  # Set up models and data for future_lapply
  bootstrap_wf <- lapply(
    yhats,
    function(yhat) {
      list(
        data = yhat$data,
        workflow = hardhat::extract_workflow(yhat$last_mod_fit)
      )
    }
  )
  
  # Set up bootstrapping
  var_ids <- rep(seq_along(bootstrap_wf), each = num_bootstrap)
  boot_ids <- rep(seq_len(num_bootstrap), times = length(bootstrap_wf))
  
  pb <- utils::txtProgressBar(min = 0, max = length(var_ids), style = 3)
  
  bootstrap_results <- future.apply::future_lapply(
    seq_along(var_ids),
    function(i, boot_fun) {
      utils::setTxtProgressBar(pb, i)
      var_id <- var_ids[i]
      boot_id <- boot_ids[i]
      mrIML_internal_bootstrap_fun(
        bootstrap_wf[[var_id]]$workflow,
        bootstrap_wf[[var_id]]$data,
        metrics = flashlight_ops$metrics,
        pred_fun = flashlight_ops$pred_fun,
        downsample_to = min_class_counts[[var_id]],
        response_name = names(yhats)[var_id],
        boot_id = boot_id
      )
    },
    future.seed = TRUE
  )
  
  # Organize bootstraps into a list
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
                                         metrics,
                                         pred_fun,
                                         downsample_to = NULL,
                                         response_name,
                                         boot_id) {
  # Resample data
  if (is.null(downsample_to)) {
    bootstrap_sample <- data %>%
      dplyr::slice(
        sample(seq_len(nrow(data)), replace = TRUE)
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
    
    if (nrow(bootstrap_sample) < 100) {
      warning(
        paste(
          "Downsampling when there are only a few observations of a particular",
          "class can cause issues further down the line in the mrIML workflow.",
          "We recommend no downsampling by default and removing responses",
          "where one class is extremely rare or common."
        )
      )
    }
  }
  
  # Refit model and create flashlight
  model_fit <- workflows::fit(
    wf,
    data = bootstrap_sample
  )
  
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
