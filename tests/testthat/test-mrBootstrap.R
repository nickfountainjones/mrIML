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

test_that("Bootstrap works for all types of classification model.", {
  skip_on_cran()
  data <- classificaiton_data()
  # multi-response
  mrIML_mr <- mrIMLpredicts(
    Y = data$Y,
    X = data$X,
    Model = model_rf_clas,
    prop = 0.7,
    k = 5
  )
  expect_no_error(
    mrBootstrap(mrIML_mr)
  )
  # graphical-network
  mrIML_gn <- mrIMLpredicts(
    Y = data$Y,
    X = data$X,
    X1 = data$X1,
    Model = model_rf_clas,
    prop = 0.7,
    k = 5
  )
  expect_no_error(
    mrBootstrap(mrIML_gn)
  )
  # co-occurance only
  mrIML_co <- mrIMLpredicts(
    Y = data$Y,
    X1 = data$X1,
    Model = model_rf_clas,
    prop = 0.7,
    k = 5
  )
  expect_no_error(
    mrBootstrap(mrIML_co)
  )
  # test with factor
  X_fact <- data$X %>%
    dplyr::mutate(
      scale.prop.zos = cut(
        scale.prop.zos,
        quantile(scale.prop.zos, probs = seq(0, 1, length.out = 4)),
        include.lowest = TRUE
      )
    )
  mrIML_gn_fact <- mrIMLpredicts(
    Y = data$Y,
    X = X_fact,
    X1 = data$X1,
    Model = model_rf_clas,
    prop = 0.7,
    k = 5,
    racing = FALSE
  )
  expect_no_error(
    mrBootstrap(mrIML_gn_fact)
  )
})
