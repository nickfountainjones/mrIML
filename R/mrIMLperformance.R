#' Calculate general performance metrics of a mrIML model
#' 
#' @description
#' Summarizes the performance of a `mrIML` object created using [mrIMLpredicts] in a way so that different models can be easily compared.
#' For regression models, root mean squared error (RMSE) and R-squared are reported, while for classification models, area under the ROC (AUC), Mathews correlation coefficient (MMC), positive predictive value (PPC), specificity, and sensitivity are reported.
#' 
#' @param mrIMLobj A list object created by [mrIMLpredicts] containing multi-response models.
#' 
#' @returns A list with two slots.
#' * `$model_performance`: A [tibble] of commonly used metric that can be used to compare model performance of classification models. Performance metrics are based on the test data defined during [mrIMLpredicts()].
#' * `$global_performance_summary`: A global performance metric: the average of a performance metric over all response models. MMC is used for classification models and RMSE for regression model. 
#' 
#' @export
#' 
#' @examples
#' 
#' library(tidymodels)
#' 
#' data <- MRFcov::Bird.parasites
#' 
#' # Prepare data for mrIML
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#' X <- data %>%
#'   select(scale.prop.zos)
#' X1 <- Y
#' 
#' # Fit GN model
#' model_rf <- rand_forest(
#'   trees = 100, # 100 trees are set for brevity. Aim to start with 1000
#'   mode = "classification",
#'   mtry = tune(),
#'   min_n = tune()
#' ) %>%
#'   set_engine("randomForest")
#'   
#' GN_model_rf <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = X1,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 5
#' )
#' 
#' perf <- mrIMLperformance(GN_model_rf)
#' perf[[1]]
#' perf[[2]]


mrIMLperformance <- function(mrIMLobj) {
  
  yhats <- mrIMLobj$Fits
  Model <- mrIMLobj$Model
  Y <- mrIMLobj$Data$Y
  mode <- mrIMLobj$Model$mode
  
  n_response <- length(yhats)
  mod_perf <- NULL
  bList <- yhats %>%
    purrr::map(
      purrr::pluck("last_mod_fit")
    )

  if (mode == "classification") {
    performance_function <- mrIMLperformance_classification
    global_metric <- "mcc"
  } else if (mode == "regression") {
    performance_function <- mrIMLperformance_regression
    global_metric <- "rmse"
  } else {
    stop(
      "mrIMLperfomance() currently only available for class \"regression\" or 
      \"classification\".",
      call. = FALSE
    )
  }
  
  model_perf <- performance_function(
    n_response,
    yhats,
    Y,
    Model,
    bList
  )
  
  global_summary <- model_perf[[global_metric]] %>%
    mean(na.rm = TRUE)
  
  return(
    list(
      model_performance = model_perf,
      global_performance_summary = global_summary
    ) 
  )

}

mrIMLperformance_classification <- function(n_response,
                                            yhats,
                                            Y,
                                            Model,
                                            bList) {
  m_perf <- lapply(
    1:n_response,
    function(i) {
      tibble::tibble(
        response = names(yhats)[i],
        model_name = class(Model)[1],
        roc_AUC = bList[[i]]$.metrics[[1]]$.estimate[2],
        mcc = bList[[i]]$.predictions[[1]] %>%
          yardstick::mcc(
            truth = class,
            estimate = .pred_class
          ) %>%
          dplyr::pull(.estimate),
        sensitivity = bList[[i]]$.predictions[[1]] %>%
          yardstick::sens(
            truth = class,
            estimate = .pred_class
          ) %>%
          dplyr::pull(.estimate),
        ppv = bList[[i]]$.predictions[[1]] %>%
          yardstick::ppv(
            truth = class,
            estimate = .pred_class
          ) %>%
          dplyr::pull(.estimate),
        specificity = bList[[i]]$.predictions[[1]] %>%
          yardstick::spec(
            truth = class,
            estimate = .pred_class
          ) %>%
          dplyr::pull(.estimate),
        prevalence = sum(Y[i]) / nrow(Y)
      )
    }
  ) %>%
    dplyr::bind_rows()
  
  # Handling for NAs in MCC
  if (any(is.na(m_perf$mcc))) {
    warning(
      paste0("NAs produced when calculating MCC. This is common when there ",
             "is a class imbalance. Substituting NA values with zero."),
      call. = FALSE
    )
    m_perf <- m_perf %>%
      dplyr::mutate(
        mcc = ifelse(is.na(mcc), 0, mcc)
      )
  }
  
  m_perf

}

mrIMLperformance_regression <- function(n_response,
                                        yhats,
                                        Y,
                                        Model,
                                        bList) {
  lapply(
    1:n_response,
    function(i) {
      tibble::tibble(
        response = names(yhats)[i],
        model_name = class(Model)[1],
        rmse = bList[[i]]$.metrics[[1]]$.estimate[1],
        rsquared = bList[[i]]$.metrics[[1]]$.estimate[2]
      )
    }
  ) %>%
    dplyr::bind_rows()
}
