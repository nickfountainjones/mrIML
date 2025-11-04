library(mrIML)
library(tidymodels)
library(flashlight)
library(ggplot2)
library(patchwork)

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

model_lm <- parsnip::logistic_reg() %>%
  set_engine("glm")



mrIML_rf <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_rf,
  prop = 0.7,
  k = 5,
  racing = TRUE
)

mrIML_lm <- mrIMLpredicts(
  X = X,
  Y = Y,
  X1 = Y,
  Model = model_lm , 
  balance_data = 'no',
  prop = 0.6,
  k = 5,
  racing = FALSE
)


perf_rf <- mrIML_rf %>%
  mrIMLperformance()
perf_rf$model_performance

perf_lm <- mrIML_lm %>%
  mrIMLperformance()
perf_rf$model_performance

perf_comp <- mrPerformancePlot(perf_rf, perf_lm)
perf_comp$performance_plot + perf_comp$performance_diff_plot

### Code Additions

mrIML_rf$Performance <- perf_rf$model_performance
mrIML_lm$Performance <- perf_lm$model_performance

### end code additions

fl_rf <- mrIML_rf %>%
  mrFlashlight()

fl_rf$Microfilaria %>%
  light_profile(data = data, v = "scale.prop.zos") %>%
  plot() +
  ggtitle("Effect of scale.prop.zos on Microfilaria") +
  theme_bw()

    
    
    