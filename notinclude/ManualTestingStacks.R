library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)
library(stacks)
library(parsnip)

set.seed(7007)

data <- MRFcov::Bird.parasites

# Responses
Y <- select(data, "Hzosteropis", "Hkillangoi", "Plas", "Microfilaria")
# Covariates
X <- select(data, "scale.prop.zos")

model_rf <- parsnip::rand_forest(
  trees = 100, # 100 trees are set for brevity. Aim to start with 1000.
  mode = "classification",
  mtry = tune(),
  min_n = tune()
) %>%
  set_engine("randomForest")
# 
# rand_forest_res <- 
#   tune_grid(
#     object = model_rf, 
#     resamples = folds, 
#     grid = 10,
#     control = ctrl_grid
#   )

mrIML_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)







