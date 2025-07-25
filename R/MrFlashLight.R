#' Convert mrIML object into a \pkg{flashlight} object
#'
#' A wrapper function around [flashlight::flashlight()] to run multi-response
#' model-agnostic interpretable machine learning analyses. The output can be
#' interrogated using the core functionality of \pkg{flashlight}: see
#' `vignette("flashlight", package = "flashlight")`.
#'
#' @param mrIMLobj A list object output by [mrIMLpredicts()].
#' @param response A character string indicating the type of response:
#' `"single"` selects one response (indicated by `index`) and `"multi"` selects
#' all responses.
#' @param index A numeric value used when `response` is `"single"` to select
#' which response column in the data to create a flashlight object for.
#' @param predict_function A function specifying a user-defined prediction
#' function (optional).
#'
#' @returns A flashlight or multi-flashlight object.
#'
#' @importMethodsFrom workflows predict.workflow
#' @examples
#' library(flashlight)
#' library(ggplot2)
#' 
#' mrIML_rf <- mrIML::mrIML_bird_parasites_RF
#'
#' fl <- mrFlashlight(
#'   mrIML_rf,
#'   response = "multi",
#'   index = 1
#' )
#' 
#' # Performance comparison
#' fl %>%
#'   light_performance(
#'     metrics = list(`ROC AUC` = MetricsWeighted::AUC)
#'   ) %>%
#'   plot() +
#'   ylim(0, 1)
#'
#' # Partial dependence curves
#' fl %>%
#'   light_profile(data = cbind(mrIML_rf$Data$X, mrIML_rf$Data$Y), "scale.prop.zos") %>%
#'   plot()
#'
#' # Two-way partial dependence
#' fl %>%
#'   light_profile2d(c("scale.prop.zos", "Plas")) %>%
#'   plot()
#' @export
mrFlashlight <- function(mrIMLobj,
                         response = "multi",
                         index = 1,
                         predict_function = NULL) {
  
  # Unpack mrIML object
  yhats <- mrIMLobj$Fits
  Model <- mrIMLobj$Model
  Y <- mrIMLobj$Data$Y
  X <- mrIMLobj$Data$X
  X1 <- mrIMLobj$Data$X1
  mode <- mrIMLobj$Model$mode
  
  # Set up flashlight functions for mode
  flashlight_ops <- mrIML_flashlight_setup(
    mode,
    predict_function
  )
  
  # Run flashlight on models
  if (response == "single") {
    mfl <- flashlight::flashlight(
      model = yhats[[index]]$last_mod_fit$.workflow[[1]],
      label = colnames(Y)[index],
      data = cbind(Y[index], X),
      y = colnames(Y)[index],
      predict_function = flashlight_ops$pred_fun,
      metrics = flashlight_ops$metrics
    )
  } else if (response == "multi") {
    fl_list <- lapply(
      seq_along(yhats),
      function(i) {
        flashlight::flashlight(
          model = yhats[[i]]$last_mod_fit$.workflow[[1]],
          label = colnames(Y)[i],
          y = colnames(Y)[i],
          x = colnames(yhats[[i]]$data)[-1]
        )
      }
    )
    mfl <- flashlight::multiflashlight(
      fl_list,
      data = cbind(Y, X),
      predict_function = flashlight_ops$pred_fun,
      metrics = flashlight_ops$metrics
    )
  } else {
    stop(
      "Response type must be either \"single\" or \"multi\".",
      call. = FALSE
    )
  }
  
  mfl
}

mrIML_flashlight_setup <- function(mode, predict_function = NULL) {
  if (mode == "classification") {
    pred_fun <- function(m, dat) {
      if (!inherits(m, "workflow")) {
        m <- m$.workflow[[1]]
        #m <- hardhat::extract_workflow(m)
      }
      pred <- m %>%
        stats::predict(
          new_data = dat,
          type = "prob"
        )
      pred %>%
        dplyr::pull(.data$.pred_1)
    }
    metrics <- list(
      logloss = MetricsWeighted::logLoss,
      `ROC AUC` = MetricsWeighted::AUC,
      `% Dev Red` = MetricsWeighted::r_squared_bernoulli
    )
  } else if (mode == "regression") {
    pred_fun <- function(m, dat) {
      if (!inherits(m, "workflow")) {
        m <- m$.workflow[[1]]
        # m <- hardhat::extract_workflow(m)
      }
      pred <- m %>%
        stats::predict(
          new_data = dat
        )
      pred %>%
        dplyr::pull(.data$.pred)
    }
    metrics <- list(
      rmse = MetricsWeighted::rmse,
      `R-squared` = MetricsWeighted::r_squared
    )
  } else {
    stop(
      paste0(
        "mrFlashlight() is currently only available for mode \"classification\" or \"regression\"."
      )
    )
  }
  
  # Override pred_fun() if user supplied one
  if (!is.null(predict_function)) pred_fun <- predict_function
  
  list(
    metrics = metrics,
    pred_fun = pred_fun
  )
}