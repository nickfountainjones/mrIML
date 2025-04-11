# Data for tests
classificaiton_data <- function() {
  data <- MRFcov::Bird.parasites
  Y <- data %>%
    dplyr::select(-scale.prop.zos) %>%
    dplyr::select(order(tidyselect::everything()))
  X <- data %>%
    dplyr::select(scale.prop.zos)
  X1 <- Y
  list(
    Y = Y,
    X = X,
    X1 = X1
  )
}

regression_data <- function() {
  data <- iris
  X <- data[5]
  Y <- data[-5]
  X1 <- Y
  list(
    Y = Y,
    X = X,
    X1 = X1
  )
}

# Model for tests
# classifier
model_rf_clas <- parsnip::rand_forest(
  trees = 100,
  mode = "classification",
  mtry = hardhat::tune(),
  min_n = hardhat::tune()
) %>%
  parsnip::set_engine("randomForest")
# regression
model_rf_reg <- parsnip::rand_forest(
  trees = 100,
  mode = "regression",
  mtry = hardhat::tune(),
  min_n = hardhat::tune()
) %>%
  parsnip::set_engine("randomForest")

test_that("both types of model run without error for classification", {
  skip_on_cran()
  data <- classificaiton_data()
  # multi-response
  expect_no_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X,
      Model = model_rf_clas,
      prop = 0.7,
      k = 5
    )
  )
  # graphical-network
  expect_no_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X,
      X1 = data$X1,
      Model = model_rf_clas,
      prop = 0.7,
      k = 5
    )
  )
})

test_that("both types of model run without error for regression", {
  skip_on_cran()
  data <- regression_data()
  # multi-response
  expect_no_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X,
      Model = model_rf_reg,
      prop = 0.7,
      k = 5
    )
  )
  # graphical-network
  expect_no_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X,
      X1 = data$X1,
      Model = model_rf_reg,
      prop = 0.7,
      k = 5
    )
  )
})

test_that("works for racing = FALSE", {
  skip_on_cran()
  data <- classificaiton_data()
  expect_no_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X,
      Model = model_rf_clas,
      prop = 0.7,
      k = 5,
      racing = FALSE
    )
  )
})
  
test_that("expected dimensions of mrIML obj", {
  data <- classificaiton_data()
  mrIMLobj_MR <- mrIMLpredicts(
    Y = data$Y,
    X = data$X,
    Model = model_rf_clas,
    prop = 0.7,
    k = 5
  )
  expect_length(mrIMLobj_MR, 3)
  expect_length(mrIMLobj_MR$Data, 3)
  expect_setequal(
    names(mrIMLobj_MR$Fits),
    names(data$Y)
  )
  expect_s3_class(mrIMLobj_MR$Model, "rand_forest")
  expect_s3_class(mrIMLobj_MR$Model, "model_spec")
})

test_that("incorect model or data inputs throw errors", {
  data <- classificaiton_data()
  expect_error(
    mrIMLpredicts(
      Y = data$Y, 
      X = data$X, 
      Model = lm(Hzosteropis ~ ., cbind(data$Y, data$X))
    ),
    "The model should be a properly specified tidymodel."
  )
  expect_error(
    mrIMLpredicts(
      Y = data$Y, 
      X = data$X[1:(nrow(data$X) - 1), ], 
      Model = model_rf_clas
    ),
    "All data inputs must have the same number of rows."
  )
  expect_error(
    mrIMLpredicts(
      Y = data$Y,
      X = data$X, 
      X1 = data$X1[1:(nrow(data$X1) - 1), ],
      Model = model_rf_clas
    ),
    "All data inputs must have the same number of rows."
  )
})

