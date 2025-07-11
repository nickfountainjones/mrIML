#' Generates a multi-response predictive model
#'
#' @description
#' This function fits separate classification/regression models, specified in
#' the \pkg{tidymodels} framework, for each response variable in a data set. This is
#' the core function of `mrIML`.
#'
#' @param Y,X,X1 Data frames containing the response, predictor, and the joint
#' response variables (i.e. the responses that are also to be used as predictors
#' if fitting GN model) respectively. If `X1` is not provided then a standard 
#' multi-response model will be fit to the data (e.g. the response models are
#' independant of one another conditional on the predictors supplied in X). See
#' **Details** section below.
#' @param Model Any model from the \pkg{tidymodels} package. See **Examples**.
#' @param balance_data A character string:
#' * "up": up-samples the data to equal class sizes.
#' * "down": down-samples the data to equal class sizes.
#' * "no": leaves the data as is. "no" is the default value.
#' @param dummy A logical value indicating if [recipes::step_dummy()] should be
#' included in the data recipe.
#' @param tune_grid_size A numeric value that sets the grid size for
#' hyperparameter tuning. Larger grid sizes increase computational time. Ignored
#' if `racing = TRUE`.
#' @param racing A logical value. If `TRUE`, `mrIML` performs the grid search
#' using the [finetune::tune_race_anova()] method; otherwise, [tune::tune_grid()]
#' is used. `racing = TRUE` is now the default method of tuning.
#' @param k A numeric value. Sets the number of folds in the cross-validation.
#' 10-fold CV is the default.
#' @param prop A numeric value between 0 and 1. Defines the training-testing
#' data proportion to be used, which defaults to `prop = 0.7`.
#'
#' @details
#' `mrIMLpredicts` fits the supplied tidy model to each response variable in the
#' data frame `Y`. If only `X` (a data frame of predictors) is supplied, then
#' independent models are fit, i.e., the other response variables are not used as
#' predictors. If `X1` (a data frame of all or select response variables) is
#' supplied, then those response variables are also used as predictors in the
#' response models. For example, supplying `X1` means that a co-occurrence model is fit.
#'
#' If `balance_data = "up"`, then [themis::step_rose()] is used to upsample the
#' dataset; however, we generally recommend using `balance_data = "no"` in most
#' cases.
#'
#' @returns A list object with three slots:
#' * `$Model`: The \pkg{tidymodels} object that was fit.
#' * `$Data`: A list of the raw data.
#' * `$Fits`: A list of the fitted models for each response variable.
#'
#' @examples
#' library(tidymodels)
#'
#' data <- MRFcov::Bird.parasites
#'
#' # Define the response variables of interest
#' Y <- data %>%
#'   select(-scale.prop.zos) %>%
#'   select(order(everything()))
#'
#' # Define the predictors
#' X <- data %>%
#'   select(scale.prop.zos)
#'
#' # Specify a random forest tidy model
#' model_rf <- rand_forest(
#'   trees = 10, # 50 trees are set for brevity. Aim to start with 1000
#'   mode = "classification",
#'   mtry = tune(),
#'   min_n = tune()
#' ) %>%
#'   set_engine("randomForest")
#'
#' # Fitting independent multi-response model -----------------------------------
#' MR_model_rf <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 2,
#'   racing = FALSE
#' )
#'
#' # Fitting a graphical network model -----------------------------------------
#' # Define the dependent response variables (all in this case)
#' X1 <- Y
#'
#' GN_model <- mrIMLpredicts(
#'   X = X,
#'   Y = Y,
#'   X1 = X1,
#'   Model = model_rf,
#'   prop = 0.7,
#'   k = 2,
#'   racing = FALSE
#' )
#'
#' @export
mrIMLpredicts <- function(X,
                          X1 = NULL,
                          Y,
                          Model,
                          balance_data = "no",
                          dummy = FALSE,
                          prop = 0.7,
                          tune_grid_size = 10,
                          k = 10,
                          racing = TRUE) {
  mode <- Model$mode
  
  # Coerce data to tibbles
  X <- tibble::as_tibble(X)
  Y <- tibble::as_tibble(Y)
  if (!is.null(X1)) X1 <- tibble::as_tibble(X1)
  
  # Check modeling inputs
  check_equal_rows(X, X1, Y)
  if (!inherits(Model, "model_spec")) {
    stop("The model should be a properly specified tidymodel.")
  }
  
  n_response <- length(Y)
  pb <- utils::txtProgressBar(min = 0, max = n_response, style = 3)
  
  yhats <- future.apply::future_lapply(
    X = seq(1, n_response),
    FUN = function(i) {
      utils::setTxtProgressBar(pb, i)
      mrIML_internal_fit_function(
        i,
        .X = X,
        .X1 = X1,
        .Y = Y,
        Model = Model,
        balance_data = balance_data,
        mode = mode,
        dummy = dummy,
        prop = prop,
        tune_grid_size = tune_grid_size,
        k = k,
        racing = racing
      )
    },
    future.seed = TRUE
  )
  
  names(yhats) <- names(Y)
  
  return(
    list(
      Model = Model,
      Data = list(
        X = X,
        Y = Y,
        X1 = X1
      ),
      Fits = yhats
    )
  )
}

mrIML_internal_fit_function <- function(i,
                                        .X,
                                        .X1,
                                        .Y,
                                        Model,
                                        balance_data,
                                        mode,
                                        dummy,
                                        prop,
                                        tune_grid_size,
                                        k,
                                        racing,
                                        seed) {
  resp_name <- names(.Y)[i]
  if (!is.null(.X1)) {
    if (!is.null(.X)) {
      data <- tibble::as_tibble(
        cbind(
          .Y %>%
            dplyr::select(resp_name),
          .X,
          .X1 %>%
            dplyr::select(-tidyselect::any_of(resp_name))
        )
      )
    } else {
      data <- tibble::as_tibble(
        cbind(
          .Y %>%
            dplyr::select(resp_name),
          .X1 %>%
            dplyr::select(-tidyselect::any_of(resp_name))
        )
      )
    }
  } else {
    if (!is.null(.X)) {
      data <- tibble::as_tibble(
        cbind(
          .Y %>%
            dplyr::select(resp_name),
          .X
        )
      )
    } else {
      stop("At least one of X or X1 must be specified.")
    }
  }
  
  # define response variable
  colnames(data)[1] <- "class"
  
  if (mode == "classification") {
    data$class <- as.factor(data$class)
  }
  
  data_split <- rsample::initial_split(data, prop = prop)
  
  # extract training and testing sets
  data_train <- rsample::training(data_split)
  data_test <- rsample::testing(data_split)
  
  # n-fold cross validation
  data_cv <- rsample::vfold_cv(data_train, v = k)
  
  # Ensure themis is installed
  if (balance_data != "no") {
    if (!requireNamespace("themis", quietly = TRUE)) {
      message(
        paste0("The 'themis' package is required if balance_data != 'no'. ",
               "Would you like to install it? (yes/no)")
      )
      response <- readline()
      if (tolower(response) == "yes") {
        utils::install.packages("themis")
      } else {
        stop(
          paste0("The 'themis' package is needed for this function. Please ",
                 "install it to proceed.")
        )
      }
    }
  }
  
  if (balance_data == "down") {
    data_recipe <- data_train %>%
      recipes::recipe(class ~ ., data = data_train) %>%
      themis::step_downsample(.data$class)
  } else if (balance_data == "up") {
    data_recipe <- data_train %>%
      recipes::recipe(class ~ ., data = data_train) %>%
      themis::step_rose(.data$class)
  } else if (balance_data == "no") {
    data_recipe <- data_train %>%
      recipes::recipe(class ~ ., data = data_train)
  }
  
  if (dummy) {
    data_recipe <- data_recipe %>%
      recipes::step_dummy(
        recipes::all_nominal(),
        -recipes::all_outcomes(),
        one_hot = TRUE
      )
  }
  
  mod_workflow <- workflows::workflow() %>%
    workflows::add_recipe(data_recipe) %>%
    workflows::add_model(Model)
  
  if (racing) {
    tune_m <- finetune::tune_race_anova(
      mod_workflow,
      resamples = data_cv
    )
  } else {
    tune_m <- tune::tune_grid(
      mod_workflow,
      resamples = data_cv,
      grid = tune_grid_size
    )
  }
  
  if (mode == "classification") {
    best_m <- tune_m %>%
      tune::select_best(metric = "roc_auc")
  } else {
    best_m <- tune_m %>%
      tune::select_best(metric = "rmse")
  }
  
  final_model <- tune::finalize_workflow(
    mod_workflow,
    best_m
  )
  
  mod1_k <- final_model %>%
    workflows::fit(data = data_train)
  
  if (mode == "classification") {
    yhatO <- stats::predict(mod1_k, new_data = data, type = "prob")
    yhat <- yhatO$.pred_1
    
    yhatT <- stats::predict(mod1_k, new_data = data_test, type = "class") %>%
      dplyr::bind_cols(
        data_test %>%
          dplyr::select("class")
      )
    
    truth <- as.numeric(as.character(data$class))
    
    deviance <- sapply(
      seq_along(truth),
      function(j) {
        resid <- ifelse(
          truth[j] == 1,
          sqrt(-2 * log(yhat[j])),
          -1 * sqrt(-2 * log(1 - yhat[j]))
        )
        return(resid)
      }
    )
    
    deviance_morans <- deviance
    deviance_morans[is.infinite(deviance_morans)] <- 2
  } else {
    yhatO <- stats::predict(mod1_k, new_data = data_train)
    yhat <- yhatO$.pred
    
    yhatT <- stats::predict(mod1_k, new_data = data_test)
    
    deviance <- NULL
  }
  
  last_mod_fit <- final_model %>%
    tune::last_fit(data_split)
  
  list(
    last_mod_fit = last_mod_fit,
    tune_m = tune_m,
    data = data,
    data_test = data_test,
    data_train = data_train,
    yhat = yhat,
    yhatT = yhatT,
    deviance = deviance
  )
}

check_equal_rows <- function(X, X1, Y) {
  input_lengths <- c(nrow(X), nrow(X1), nrow(Y))
  input_lengths <- input_lengths[!is.na(input_lengths)]
  
  unique_nrow_vals <- input_lengths %>%
    unique() %>%
    length()
  
  if (unique_nrow_vals != 1) {
    stop("All data inputs must have the same number of rows.", call. = FALSE)
  }
}