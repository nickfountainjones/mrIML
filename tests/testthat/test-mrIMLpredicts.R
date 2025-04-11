# Data for tests
data <- MRFcov::Bird.parasites
Y <- data %>%
  dplyr::select(-scale.prop.zos) %>%
  dplyr::select(order(tidyselect::everything()))
X <- data %>%
  dplyr::select(scale.prop.zos)
X1 <- Y
# Model for tests
model_rf <- parsnip::rand_forest(
  trees = 100,
  mode = "classification",
  mtry = hardhat::tune(),
  min_n = hardhat::tune()
) %>%
  parsnip::set_engine("randomForest")

mrIMLobj_MR <- mrIMLpredicts(
  Y = Y,
  X = X,
  Model = model_rf,
  prop = 0.7,
  k = 5
)

test_that("both types of model run without error", {
  skip_on_cran()
  expect_no_error(
    mrIMLpredicts(
      Y = Y,
      X = X,
      Model = model_rf,
      prop = 0.7,
      k = 5
    )
  )
  expect_no_error(
    mrIMLpredicts(
      Y = Y,
      X = X,
      X1 = X1,
      Model = model_rf,
      prop = 0.7,
      k = 5
    )
  )
})

test_that("expected dimensions of mrIML obj", {
  expect_length(mrIMLobj_MR, 3)
  expect_length(mrIMLobj_MR$Data, 3)
  expect_setequal(
    names(mrIMLobj_MR$Fits),
    names(Y)
  )
  expect_s3_class(mrIMLobj_MR$Model, "rand_forest")
  expect_s3_class(mrIMLobj_MR$Model, "model_spec")
})

test_that("incorect model or data inputs throw errors", {
  expect_error(
    mrIMLpredicts(Y = Y, X = X, Model = lm(Hzosteropis ~ ., cbind(Y, X))),
    "The model should be a properly specified tidymodel."
  )
  expect_error(
    mrIMLpredicts(Y = Y, X = X[1:(nrow(X) - 1), ], Model = model_rf),
    "All data inputs must have the same number of rows."
  )
  expect_error(
    mrIMLpredicts(Y = Y, X = X, X1 = X1[1:(nrow(X1) - 1), ], Model = model_rf),
    "All data inputs must have the same number of rows."
  )
})

