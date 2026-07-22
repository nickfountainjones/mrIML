library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)
library(stacks)
library(parsnip)

set.seed(7007)

data <- as_tibble(MRFcov::Bird.parasites)

### For sample stacks we will go with Hzosteropis first:

data$Hzosteropis <- as.factor(data$Hzosteropis)
data$Hkillangoi <- as.factor(data$Hkillangoi)
data$Plas <- as.factor(data$Plas)
data$Microfilaria <- as.factor(data$Microfilaria)


YdStack <- select(data, "Hzosteropis")
XdStack <- select(data, -"Hzosteropis")




dStack_split <- initial_split(data)
dStack_train <- training(dStack_split)
dStack_test  <- testing(dStack_split)

folds <- rsample::vfold_cv(dStack_train,v=5)

hzStack_recipe <- recipe(Hzosteropis ~ ., data = dStack_train) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors())

hzStack_wflow <- workflow() %>%
  add_recipe(hzStack_recipe)

ctrl_grid <- stacks::control_stack_grid()

rand_forest_spec <- 
  rand_forest(
    mtry = tune(),
    min_n = tune(),
    trees = 500
  ) %>%
  set_mode("classification") %>%
  set_engine("randomForest")

rand_forest_wflow <-
  hzStack_wflow |>
  add_model(rand_forest_spec)

rand_forest_res <- 
  tune_grid(
    object = rand_forest_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )


nnet_spec <-
  mlp(hidden_units = tune(), penalty = tune(), epochs = tune()) |>
  set_mode("classification") |>
  set_engine("nnet")
# 
nnet_rec <- 
  hzStack_recipe |> 
  step_normalize(all_predictors())

nnet_wflow <- 
  hzStack_wflow |>
  add_model(nnet_spec) |>
  update_recipe(nnet_rec)

nnet_res <-
  tune_grid(
    object = nnet_wflow, 
    resamples = folds, 
    grid = 10,
    control = ctrl_grid
  )

lm_spec <- parsnip::logistic_reg(engine = 'glm'
                                 ,mode = 'classification')

lm_wflow <- hzStack_wflow %>%
  add_model(lm_spec)
  

lm_res <- tune_grid(object = lm_wflow
                    , resamples = folds
                    , grid = 10
                    , control = ctrl_grid)


hzStack_model_st <- stacks() %>%
  add_candidates(rand_forest_res) %>%
  add_candidates(nnet_res) %>%
  add_candidates(lm_res) %>%
  blend_predictions() %>%
  fit_members()

tempCollection <- collect_parameters(hzStack_model_st, "rand_forest_res")

stackPred_pred <- dStack_test %>%
bind_cols(predict(hzStack_model_st, ., type = "prob"))

sTyhat0 <- stats::predict(hzStack_model_st, new_data = dStack_test, type = "class")
sTyhat <- sTyhat0$.pred_class

#sTyhat0 <- stats::predict(hzStack_model_st, new_data = dStack_test, type = "prob")
#sTyhat <- sTyhat0$.pred_1

checkArray <- bind_cols(dplyr::select(dStack_test,"Hzosteropis"),sTyhat)
checkArray2 <- tibble(dplyr::select(dStack_test,"Hzosteropis")
                      ,Estimated = sTyhat)

# tempMetrics <- yardstick::roc_auc(
#   stackPred_pred,
#   truth = Hzosteropis,
#   estimator = contains(".pred_")
# )



tempMetrics <- yardstick::accuracy(checkArray2
                                  , Hzosteropis
                                  , Estimated)
#   stackPred_pred,
#   truth = stackPred_pred$Hzosteropis,
#   estimator = stackPred_pred$.pred_0
#   )












